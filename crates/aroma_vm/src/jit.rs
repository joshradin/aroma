//! Used for creating just in time compiled sources for the avm

use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};

use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::prelude::types::{F32, F64, I32, I64, I8, R64};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError};
use log::trace;
use parking_lot::Mutex;

use aroma_bytecode::chunk::Constant;
use aroma_bytecode::chunk::OpCode;
use aroma_bytecode::chunk::UnknownOpcode;

use crate::jit::ir::{
    Block as IrBlockId, CompileIrError, IrBlock, IrCompiler, IrFunction, IrOp, IrValue,
};
use crate::types::function::{FnSignature, ObjFunction};
use crate::types::{Type as AromaType, Value as AromaValue};
use crate::vm::StaticFunctionTable;

pub mod abi;
pub mod ir;

/// JIT compiler, converting [ObjFunction](crate::types::function::ObjFunction) function objects into
/// compiled machine code.
pub struct JIT {
    /// The function builder context, which is reused across multiple [FunctionBuilder] instances
    builder_context: FunctionBuilderContext,
    /// The main [cranelift] context, which holds the state of the codegen. This is seperate from [Module]
    /// to allow for parallel compilation.
    ctx: codegen::Context,
    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
    func_refs: HashMap<String, ObjFunction>,
    func_ids: HashMap<String, FuncId>,
    ir_compiler: Mutex<IrCompiler>,
}

/// Settings used for configuring JIT compilation
#[derive(Debug)]
pub struct JITConfig {
    /// the threshold that must be met for JIT compilation to occur for a given
    pub threshold: usize,
}

impl Default for JITConfig {
    fn default() -> Self {
        Self { threshold: 10_000 }
    }
}

impl Debug for JIT {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JIT").finish_non_exhaustive()
    }
}

impl JIT {
    /// Creates a new JIT
    pub fn new(function_defs: &StaticFunctionTable) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder()
            .unwrap_or_else(|msg| panic!("host machine is not supported: {msg}"));
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
            func_refs: Default::default(),
            func_ids: Default::default(),
            ir_compiler: Mutex::new(IrCompiler::new(function_defs)),
        }
    }

    /// Compiles the function into machine code
    pub fn compile(&mut self, func: &ObjFunction) -> JitResult<*const u8> {
        let id = self.translate(func)?;

        self.module.define_function(id, &mut self.ctx)?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);
        Ok(code)
    }

    fn translate(&mut self, func: &ObjFunction) -> JitResult<FuncId> {
        trace!("translating {func:?} to JIT code via cranelift-codegen");
        for param_ty in func.params_ty() {
            let abi = Self::get_abi_param(param_ty)?;
            self.ctx.func.signature.params.push(abi);
        }
        if let Some(ret) = func.return_ty() {
            self.ctx
                .func
                .signature
                .returns
                .push(Self::get_abi_param(ret)?);
        }
        let id =
            self.module
                .declare_function(func.name(), Linkage::Export, &self.ctx.func.signature)?;
        trace!("declared function with id {id}");

        self.func_refs
            .entry(func.name().to_string())
            .or_insert(func.clone());
        self.func_ids.entry(func.name().to_string()).or_insert(id);
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let (variables, local_to_index) = declare_variables(&mut builder, func, entry_block);
        trace!("declared variables {variables:?} with mapping {local_to_index:?}");

        let mut trans = IrTranslator {
            builder,
            module: &mut self.module,
            variables,
            local_var_to_idx: local_to_index,
            func_refs: &self.func_refs,
            func_ids: &self.func_ids,
            ir_value_to_cranelift_value: Default::default(),
            ir_block_to_cranelift_block: Default::default(),
            visited: Default::default(),
        };

        let ir = self.ir_compiler.lock().compile(func)?;
        trans.translate_func(ir, func)?;

        trace!("{:#?}", trans.builder.func);
        trans.builder.finalize();
        Ok(id)
    }

    fn get_abi_param(ty: &AromaType) -> JitResult<AbiParam> {
        use AromaType::*;
        match ty {
            Int => Ok(AbiParam::new(I32)),
            Long => Ok(AbiParam::new(I64)),
            Float => Ok(AbiParam::new(F32)),
            Double => Ok(AbiParam::new(F64)),
            Byte => Ok(AbiParam::new(I8)),
            Boolean => Ok(AbiParam::new(I8.as_truthy())),
            Object { .. } | String => Ok(AbiParam::new(R64)),
            default => Err(JitError::UnsupportedParam(default.clone())),
        }
    }

    fn get_abi_type(ty: &AromaType) -> JitResult<Type> {
        use AromaType::*;
        match ty {
            Int => Ok(I32),
            Long => Ok(I64),
            Float => Ok(F32),
            Double => Ok(F64),
            Byte => Ok(I8),
            Boolean => Ok(I8.as_truthy()),
            Object { .. } | String => Ok(R64),
            default => Err(JitError::UnsupportedParam(default.clone())),
        }
    }
}

/// Bytecode translate.
///
/// You can think of the generated bytecode as a sequence of operators and operands
/// in reverse polish notation.
struct IrTranslator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
    variables: HashMap<usize, Variable>,
    local_var_to_idx: HashMap<usize, usize>,
    func_refs: &'a HashMap<String, ObjFunction>,
    func_ids: &'a HashMap<String, FuncId>,
    ir_value_to_cranelift_value: HashMap<IrValue, Value>,
    ir_block_to_cranelift_block: HashMap<IrBlockId, Block>,
    visited: HashSet<IrBlockId>,
}

impl<'a> IrTranslator<'a> {
    fn translate_func(&mut self, ir: IrFunction, f: &ObjFunction) -> JitResult<Block> {
        trace!("converted {:?} to ir", f.name());

        trace!("adding bridge instructions from manually generated block to IR blocks...");
        let block = self.builder.create_block();
        let entry = &ir.blocks()[&ir.entry()];
        self.ir_block_to_cranelift_block.insert(ir.entry(), block);
        let mut entry_params = vec![];
        for (i, _) in entry.parameters().iter().enumerate() {
            entry_params.push(self.builder.use_var(*self.variables.get(&i).unwrap()))
        }

        self.builder.ins().jump(block, &entry_params);
        trace!("bridge completed, continuing on to ir");
        self.translate_ir_block(block, entry, &ir)?;

        self.builder.seal_all_blocks();

        Ok(block)
    }

    fn translate_ir_block(
        &mut self,
        block: Block,
        ir_block: &IrBlock,
        func: &IrFunction,
    ) -> JitResult<()> {
        trace!("translating {ir_block:#?}");
        self.builder.switch_to_block(block);

        for ir_value in ir_block.parameters() {
            trace!("adding block param for {ir_value:?}");
            let value = self
                .builder
                .append_block_param(block, JIT::get_abi_type(ir_value.get_type())?);
            self.ir_value_to_cranelift_value
                .insert(ir_value.clone(), value);
        }

        for (val, op) in ir_block.ops() {
            trace!("translating {val:?} = {op}");
            self.translate_op(block, val, op, func)?;
        }

        Ok(())
    }

    fn translate_op(
        &mut self,
        block: Block,
        val: &IrValue,
        op: &IrOp,
        func: &IrFunction,
    ) -> JitResult<()> {
        match op {
            IrOp::Constant(c) => {
                let v = match c {
                    AromaValue::Long(l) => self.builder.ins().iconst(I64, *l),
                    AromaValue::Int(i) => self.builder.ins().iconst(I32, *i as i64),
                    AromaValue::Char(c) => {
                        todo!("utf-8 char representation")
                    }
                    AromaValue::Boolean(b) => self
                        .builder
                        .ins()
                        .iconst(I8.as_truthy(), if *b { 1 } else { 0 }),
                    AromaValue::Byte(b) => self.builder.ins().iconst(I8, *b as i64),
                    AromaValue::String(_) => {
                        todo!("string representation")
                    }
                    AromaValue::Double(d) => self.builder.ins().f64const(*d),
                    AromaValue::Float(f) => self.builder.ins().f32const(*f),
                    _ => {
                        todo!("object representation")
                    }
                };
                self.ir_value_to_cranelift_value.insert(val.clone(), v);
            }
            IrOp::Function(func) => {
                let func_id = *self.func_ids.get(func).expect("no function found");
                let v = self.module.declare_func_in_func(func_id, self.builder.func);
                let value = self.builder.ins().func_addr(I64, v);
                self.ir_value_to_cranelift_value.insert(val.clone(), value);
            }
            IrOp::Return(ret) => {
                if let Some(ret_val) = ret {
                    let ret = *self.ir_value_to_cranelift_value.get(ret_val).unwrap();
                    self.builder.ins().return_(&[ret]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            IrOp::Assign(var_idx, ir_val) => {
                let variable = Variable::new(*var_idx);
                let val = *self.ir_value_to_cranelift_value.get(ir_val).unwrap();
                self.builder.def_var(variable, val);
            }
            IrOp::BinOp(opcode, a, b) => {
                let jit_val = match opcode {
                    OpCode::Lt => self.translate_icmp(IntCC::SignedLessThan, a, b),
                    OpCode::Gt => self.translate_icmp(IntCC::SignedGreaterThan, a, b),
                    OpCode::Gte => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, a, b),
                    OpCode::Lte => self.translate_icmp(IntCC::SignedLessThanOrEqual, a, b),
                    OpCode::Eq => self.translate_icmp(IntCC::Equal, a, b),
                    OpCode::Neq => self.translate_icmp(IntCC::NotEqual, a, b),
                    OpCode::Add => {
                        let a = *self.ir_value_to_cranelift_value.get(a).unwrap();
                        let b = *self.ir_value_to_cranelift_value.get(b).unwrap();
                        Ok(self.builder.ins().iadd(a, b))
                    }
                    OpCode::Mult => {
                        let a = *self.ir_value_to_cranelift_value.get(a).unwrap();
                        let b = *self.ir_value_to_cranelift_value.get(b).unwrap();
                        Ok(self.builder.ins().imul(a, b))
                    }
                    OpCode::Divide => {
                        let a = *self.ir_value_to_cranelift_value.get(a).unwrap();
                        let b = *self.ir_value_to_cranelift_value.get(b).unwrap();
                        Ok(self.builder.ins().sdiv(a, b))
                    }
                    OpCode::Subtract => {
                        let a = *self.ir_value_to_cranelift_value.get(a).unwrap();
                        let b = *self.ir_value_to_cranelift_value.get(b).unwrap();
                        Ok(self.builder.ins().isub(a, b))
                    }
                    _ => unreachable!("invalid binop"),
                }?;
                self.ir_value_to_cranelift_value
                    .insert(val.clone(), jit_val);
            }
            IrOp::GetLocal(var_idx) => {
                let variable = Variable::new(*var_idx);
                let v = self.builder.use_var(variable);
                self.ir_value_to_cranelift_value.insert(val.clone(), v);
            }
            IrOp::If {
                cond,
                then_block: ir_then_block,
                then_params,
                else_block: ir_else_block,
                else_params,
            } => {
                let cond_value = *self.ir_value_to_cranelift_value.get(cond).unwrap();
                let then_params = then_params
                    .iter()
                    .map(|v| *self.ir_value_to_cranelift_value.get(v).unwrap())
                    .collect::<Vec<_>>();
                let else_params = else_params
                    .iter()
                    .map(|v| *self.ir_value_to_cranelift_value.get(v).unwrap())
                    .collect::<Vec<_>>();

                let then_block = *self
                    .ir_block_to_cranelift_block
                    .entry(*ir_then_block)
                    .or_insert_with(|| self.builder.create_block());
                let else_block = *self
                    .ir_block_to_cranelift_block
                    .entry(*ir_else_block)
                    .or_insert_with(|| self.builder.create_block());

                self.builder.ins().brif(
                    cond_value,
                    then_block,
                    &then_params,
                    else_block,
                    &else_params,
                );

                if !self.visited.contains(ir_then_block) {
                    self.visited.insert(ir_then_block.clone());
                    self.translate_ir_block(then_block, &func.blocks()[ir_then_block], func)?;
                }
                if !self.visited.contains(ir_else_block) {
                    self.visited.insert(ir_else_block.clone());
                    self.translate_ir_block(else_block, &func.blocks()[ir_else_block], func)?;
                }
            }
            IrOp::Jump {
                block: ir_block,
                block_params,
            } => {
                let block_params = block_params
                    .iter()
                    .map(|v| *self.ir_value_to_cranelift_value.get(v).unwrap())
                    .collect::<Vec<_>>();

                let next_block = *self
                    .ir_block_to_cranelift_block
                    .entry(*ir_block)
                    .or_insert_with(|| self.builder.create_block());

                self.builder.ins().jump(next_block, &block_params);
                if !self.visited.contains(ir_block) {
                    self.visited.insert(ir_block.clone());
                    self.translate_ir_block(next_block, &func.blocks()[ir_block], func)?;
                }
            }
            IrOp::Call(callee, values) => {
                let callee_signature = callee
                    .get_type()
                    .as_fn_signature()
                    .expect("must have function signature");
                let callee = *self.ir_value_to_cranelift_value.get(callee).unwrap();
                let values = values
                    .iter()
                    .map(|s| *self.ir_value_to_cranelift_value.get(s).unwrap())
                    .collect::<Vec<_>>();

                let signature = self.as_signature(callee_signature)?;
                let sig_ref = self.builder.import_signature(signature);

                trace!(
                    "creating call_indirect with {}, {}, {:?}",
                    sig_ref,
                    callee,
                    values
                );

                let return_value = self.builder.ins().call_indirect(sig_ref, callee, &values);
                if callee_signature.ret().is_some() {
                    let v = self.builder.inst_results(return_value)[0];
                    self.ir_value_to_cranelift_value.insert(val.clone(), v);
                }
            }
            _other => unimplemented!("{_other:?} translation"),
        }
        Ok(())
    }

    fn translate_icmp(&mut self, op: IntCC, l: &IrValue, r: &IrValue) -> JitResult<Value> {
        let l = *self.ir_value_to_cranelift_value.get(l).unwrap();
        let r = *self.ir_value_to_cranelift_value.get(r).unwrap();
        Ok(self.builder.ins().icmp(op, l, r))
    }
    //
    // fn translate_call(&mut self, callee: IrOp, args: Vec<IrOp>) -> JitResult<Value> {
    //     let mut arg_values = vec![];
    //     for arg in args {
    //         arg_values.push(self.translate_expr(arg)?);
    //     }
    //
    //     if let IrOp::Function(func_name) = callee {
    //         let func_id = self
    //             .func_ids
    //             .get(&func_name)
    //             .ok_or_else(|| JitError::UndefinedFunction(func_name))?;
    //         let func_ref = self
    //             .module
    //             .declare_func_in_func(*func_id, self.builder.func);
    //         let call = self.builder.ins().call(func_ref, &arg_values);
    //         Ok(self.builder.inst_results(call)[0])
    //     } else {
    //         let fn_signature = self.get_signature(&callee)?;
    //         let callee_v = self.translate_expr(callee)?;
    //         let signature = self.as_signature(&fn_signature)?;
    //         let sig_ref = self.builder.import_signature(signature);
    //
    //         let call = self
    //             .builder
    //             .ins()
    //             .call_indirect(sig_ref, callee_v, &arg_values);
    //         Ok(self.builder.inst_results(call)[0])
    //     }
    // }
    //
    // fn get_signature(&mut self, op: &IrOp) -> JitResult<FnSignature> {
    //     match op {
    //         IrOp::Function(f) => {
    //             let f = self
    //                 .func_refs
    //                 .get(f)
    //                 .ok_or(JitError::UndefinedFunction(f.to_string()))?;
    //             Ok(f.signature())
    //         }
    //         other => {
    //             panic!("can not get fn signature for this {other:?}")
    //         }
    //     }
    // }

    fn as_signature(&mut self, sig: &FnSignature) -> JitResult<Signature> {
        let mut building = self.module.make_signature();
        for param in sig.parameters() {
            let abi_type = JIT::get_abi_param(param)?;
            building.params.push(abi_type);
        }
        if let Some(ret_ty) = sig.ret() {
            let abi_type = JIT::get_abi_param(ret_ty)?;
            building.returns.push(abi_type);
        }

        Ok(building)
    }
}

fn declare_variables(
    builder: &mut FunctionBuilder,
    function: &ObjFunction,
    entry_block: Block,
) -> (HashMap<usize, Variable>, HashMap<usize, usize>) {
    let mut variables = HashMap::new();
    let mut local_to_index = HashMap::new();
    let mut index = 0;

    for (i, ty) in function.params_ty().iter().enumerate() {
        local_to_index.insert(i, index);
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(builder, &mut variables, &mut index, ty);
        builder.def_var(var, val);
    }

    for (i, ty) in function.variables().iter().enumerate() {
        local_to_index.insert(i + function.arity(), index);
        declare_variable(builder, &mut variables, &mut index, ty);
    }

    (variables, local_to_index)
}

fn declare_variable(
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<usize, Variable>,
    index: &mut usize,
    ty: &AromaType,
) -> Variable {
    let var_idx = *index;
    let var = Variable::new(var_idx);
    if !variables.contains_key(&var_idx) {
        variables.insert(var_idx, var);
        builder.declare_var(var, JIT::get_abi_type(ty).unwrap());
        *index += 1;
    }
    var
}

#[derive(Debug, thiserror::Error)]
pub enum JitError {
    #[error(transparent)]
    ModuleError(#[from] ModuleError),
    #[error("Unsupported type for JIT: {0:?}")]
    UnsupportedParam(crate::types::Type),
    #[error(transparent)]
    UnknownOpcode(#[from] UnknownOpcode),
    #[error("Can't currently represent {0} in ir")]
    UnrepresentableConstant(Constant),
    #[error("No constant at index {0}")]
    NoConstant(u8),
    #[error("Illegal operation: {0}")]
    IllegalOp(String),
    #[error("Function {0:?} is not defined")]
    UndefinedFunction(String),
    #[error(transparent)]
    CompileIrError(#[from] CompileIrError),
}

impl JitError {
    pub fn illegal_op<S: AsRef<str>>(op: S) -> Self {
        Self::IllegalOp(op.as_ref().to_string())
    }
}

pub type JitResult<T> = std::result::Result<T, JitError>;

#[cfg(test)]
mod tests {
    use std::mem;
    use std::sync::Arc;

    use test_log::test;

    use crate::examples::{factorial, fibonacci};
    use crate::function;
    use crate::jit::JIT;
    use crate::types::Type;
    use crate::vm::StaticFunctionTable;

    #[test]
    fn test_jit_factorial() {
        let factorial = factorial();
        let jit = JIT::new(&StaticFunctionTable::default())
            .compile(&factorial)
            .expect("could not compile");
    }

    #[test]
    fn test_jit() {
        let sum = function!(
            name "sum2",
            params (Type::Int),
            ret Type::Int,
            variables (Type::Int),
            bytecode {
                lset(0_u8)
                lget(0_u8)
                lget(0_u8)
                mul
                lget(0_u8)
                add
                ret
            }
        );

        let jit = JIT::new(&StaticFunctionTable::default())
            .compile(&sum)
            .expect("could not compile");
    }

    #[test]
    fn test_jit_fibonacci() {
        let fibonacci = Arc::new(fibonacci());

        let functions = StaticFunctionTable::default();
        functions
            .write()
            .insert("fibonacci".to_string(), fibonacci.clone());
        let jit = JIT::new(&functions)
            .compile(&fibonacci)
            .expect("could not compile");
    }

    fn sum(i: i32) -> i32 {
        sum2(i) + sum2(i)
    }

    fn sum2(i: i32) -> i32 {
        i * i + i
    }

    #[test]
    fn test_jit_multiple() {
        let sum2_func = Arc::new(function!(
            name "sum2",
            params (Type::Int),
            ret Type::Int,
            variables (Type::Int),
            bytecode {
                lset(0_u8)
                lget(0_u8)
                lget(0_u8)
                mul
                lget(0_u8)
                add
                ret
            }
        ));
        let sum_func = Arc::new(function!(
            name "sum",
            params (Type::Int),
            ret Type::Int,
            variables (),
            consts {
                utf8 "sum2"
                function_ref 0
            }
            bytecode {
                lset(0_u8)
                lget(0_u8)
                const(1_u8)
                call(1_u8)
                lget(0_u8)
                const(1_u8)
                call(1_u8)
                add
                ret
            }
        ));

        let function_table = StaticFunctionTable::default();
        function_table
            .write()
            .insert("sum2".to_string(), sum2_func.clone());
        function_table
            .write()
            .insert("sum".to_string(), sum_func.clone());
        let mut jit = JIT::new(&function_table);
        let _ = jit.compile(&sum2_func).expect("could not compile");
        let r = jit.compile(&sum_func).expect("could not compile");

        unsafe {
            let emit: i32 = run_code(r, 15);
            println!("emit: {emit}");
            assert_eq!(emit, sum(15));
        }
    }

    unsafe fn run_code<I, O>(ptr: *const u8, input: I) -> O {
        let code_fn = mem::transmute::<_, fn(I) -> O>(ptr);
        code_fn(input)
    }
}
