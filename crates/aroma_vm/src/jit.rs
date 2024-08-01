//! Used for creating just in time compiled sources for the avm

use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use cranelift::prelude::*;
use cranelift::prelude::types::{F32, F64, I32, I64, I8, R64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError};

use crate::chunk::{Constant, OpCode, UnknownOpcode};
use crate::jit::ir::{Op, to_ir};
use crate::types::{FnSignature, Type as AromaType};
use crate::types::function::ObjFunction;
use crate::types::Value as AromaValue;

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
}

impl Debug for JIT {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JIT").finish_non_exhaustive()
    }
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("regalloc_verbose_logs", "true").unwrap();
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
        }
    }
}

impl JIT {
    /// Creates a new JIT
    #[inline]
    pub fn new() -> Self {
        Self::default()
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

        let mut trans = BytecodeTranslator {
            builder,
            module: &mut self.module,
            variables,
            local_var_to_idx: local_to_index,
            func_refs: &self.func_refs,
            func_ids: &self.func_ids,
        };

        trans.translate_bytecode(func)?;

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
struct BytecodeTranslator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
    variables: HashMap<usize, Variable>,
    local_var_to_idx: HashMap<usize, usize>,
    func_refs: &'a HashMap<String, ObjFunction>,
    func_ids: &'a HashMap<String, FuncId>,
}

impl<'a> BytecodeTranslator<'a> {
    fn translate_bytecode(&mut self, f: &ObjFunction) -> JitResult<()> {
        let ir = to_ir(f)?;

        for op in ir {
            self.translate_statement(op)?;
        }

        Ok(())
    }

    fn translate_statement(&mut self, op: Op) -> JitResult<()> {
        match op {
            Op::Assign(var, op) => {
                let new_value = self.translate_expr(*op)?;
                let variable = self.variables.get(&var).unwrap();
                self.builder.def_var(*variable, new_value);
            }
            Op::Return(op) => {
                let value = self.translate_expr(*op)?;
                self.builder.ins().return_(&[value]);
            }
            Op::If {
                cond,
                then,
                otherwise,
            } => {
                self.translate_if_else(*cond, then, otherwise)?;
            }
            op => {
                unimplemented!("JIT compilation for {op:?}")
            }
        }
        Ok(())
    }

    fn translate_if_else(
        &mut self,
        condition: Op,
        then_body: Vec<Op>,
        else_body: Vec<Op>,
    ) -> JitResult<()> {
        let condition_value = self.translate_expr(condition)?;

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(condition_value, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        for x in then_body {
            self.translate_statement(x)?;
        }
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        for x in else_body {
            self.translate_statement(x)?;
        }

        Ok(())
    }

    fn translate_expr(&mut self, op: Op) -> JitResult<Value> {
        match op {
            Op::Param(idx) => {
                let variable = self.variables.get(&idx).unwrap();
                Ok(self.builder.use_var(*variable))
            }
            Op::GetLocal(idx) => {
                let variable = self.variables.get(&self.local_var_to_idx[&idx]).unwrap();
                Ok(self.builder.use_var(*variable))
            }
            Op::Constant(v) => match v {
                AromaValue::Long(l) => Ok(self.builder.ins().iconst(I64, l)),
                AromaValue::Int(i) => Ok(self.builder.ins().iconst(I32, i as i64)),
                AromaValue::Char(c) => {
                    todo!("utf-8 char representation")
                }
                AromaValue::Boolean(b) => Ok(self
                    .builder
                    .ins()
                    .iconst(I8.as_truthy(), if b { 1 } else { 0 })),
                AromaValue::Byte(b) => Ok(self.builder.ins().iconst(I8, b as i64)),
                AromaValue::String(_) => {
                    todo!("string representation")
                }
                AromaValue::Double(d) => Ok(self.builder.ins().f64const(d)),
                AromaValue::Float(f) => Ok(self.builder.ins().f32const(f)),
                _ => {
                    todo!("object representation")
                }
            },
            Op::BinOp(opcode, a, b) => match opcode {
                OpCode::Lt => self.translate_icmp(IntCC::SignedLessThan, *a, *b),
                OpCode::Gt => self.translate_icmp(IntCC::SignedGreaterThan, *a, *b),
                OpCode::Gte => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *a, *b),
                OpCode::Lte => self.translate_icmp(IntCC::SignedLessThanOrEqual, *a, *b),
                OpCode::Eq => self.translate_icmp(IntCC::Equal, *a, *b),
                OpCode::Neq => self.translate_icmp(IntCC::NotEqual, *a, *b),
                OpCode::Add => {
                    let a = self.translate_expr(*a)?;
                    let b = self.translate_expr(*b)?;
                    Ok(self.builder.ins().iadd(a, b))
                }
                OpCode::Mult => {
                    let a = self.translate_expr(*a)?;
                    let b = self.translate_expr(*b)?;
                    Ok(self.builder.ins().imul(a, b))
                }
                OpCode::Subtract => {
                    let a = self.translate_expr(*a)?;
                    let b = self.translate_expr(*b)?;
                    Ok(self.builder.ins().isub(a, b))
                }
                _ => unreachable!("invalid binop"),
            },
            Op::Call(callee, args) => self.translate_call(*callee, args),
            Op::Function(function) => {
                let func = if let Some(id) = self.func_ids.get(&function) {
                    self.module.declare_func_in_func(*id, self.builder.func)
                } else {
                    panic!("external function")
                };
                Ok(self.builder.ins().func_addr(R64, func))
            }
            op => {
                unimplemented!("JIT compilation for expr {op:?}")
            }
        }
    }

    fn translate_icmp(&mut self, op: IntCC, l: Op, r: Op) -> JitResult<Value> {
        let l = self.translate_expr(l)?;
        let r = self.translate_expr(r)?;
        Ok(self.builder.ins().icmp(op, l, r))
    }

    fn translate_call(&mut self, callee: Op, args: Vec<Op>) -> JitResult<Value> {
        let mut arg_values = vec![];
        for arg in args {
            arg_values.push(self.translate_expr(arg)?);
        }

        if let Op::Function(func_name) = callee {
            let func_id = self
                .func_ids
                .get(&func_name)
                .ok_or_else(|| JitError::UndefinedFunction(func_name))?;
            let func_ref = self
                .module
                .declare_func_in_func(*func_id, self.builder.func);
            let call = self.builder.ins().call(func_ref, &arg_values);
            Ok(self.builder.inst_results(call)[0])
        } else {
            let fn_signature = self.get_signature(&callee)?;
            let callee_v = self.translate_expr(callee)?;
            let signature = self.as_signature(&fn_signature)?;
            let sig_ref = self.builder.import_signature(signature);

            let call = self
                .builder
                .ins()
                .call_indirect(sig_ref, callee_v, &arg_values);
            Ok(self.builder.inst_results(call)[0])
        }
    }

    fn get_signature(&mut self, op: &Op) -> JitResult<FnSignature> {
        match op {
            Op::Function(f) => {
                let f = self
                    .func_refs
                    .get(f)
                    .ok_or(JitError::UndefinedFunction(f.to_string()))?;
                Ok(f.signature())
            }
            other => {
                panic!("can not get fn signature for this {other:?}")
            }
        }
    }

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

    use crate::examples::fibonacci;
    use crate::function;
    use crate::jit::JIT;
    use crate::types::Type;

    #[test]
    fn test_jit_factorial() {
        let factorial = function!(
            name "factorial",
            params (Type::Int),
            ret Type::Int,
            variables (),
            consts {
                int 0
                int 1
                utf8 "factorial"
                function_ref 2
            }
            bytecode {
                lset(0_u8)
                lget(0_u8)
                const(0_u8)
                eq
                jz(4_u16)
                pop
                const(1_u8)
                ret
                pop
                lget(0_u8)
                const(1_u8)
                sub
                const(3_u8)
                call(1_u8)
                lget(0_u8)
                mul
                ret
            }
        );

        let jit = JIT::new().compile(&factorial).expect("could not compile");
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

        let jit = JIT::new().compile(&sum).expect("could not compile");
    }

    #[test]
    fn test_jit_fibonacci() {
        let factorial = fibonacci();

        let jit = JIT::new().compile(&factorial).expect("could not compile");
    }

    fn sum(i: i32) -> i32 {
        sum2(i) + sum2(i)
    }

    fn sum2(i: i32) -> i32 {
        i * i + i
    }

    #[test]
    fn test_jit_multiple() {
        let sum2_func = function!(
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
        let sum_func = function!(
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
        );

        let mut jit = JIT::new();
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
