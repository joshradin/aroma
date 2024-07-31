//! Used for creating just in time compiled sources for the avm

use std::collections::HashMap;
use std::sync::Arc;

use cranelift::prelude::*;
use cranelift::prelude::types::{F32, F64, I32, I64, I8, R64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module, ModuleError};

use crate::chunk::{Chunk, OpCode, UnknownOpcode};
use crate::jit::ir::to_ir;
use crate::types::function::ObjFunction;
use crate::types::Type as AromaType;
use crate::types::Type::{Boolean, Byte, Double, Float, Int, Long, String};

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
}

impl Default for JIT {
    fn default() -> Self {
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
        self.translate(func)?;

        let id =
            self.module
                .declare_function(func.name(), Linkage::Export, &self.ctx.func.signature)?;

        self.module.define_function(id, &mut self.ctx)?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);
        Ok(code)
    }

    fn translate(&mut self, func: &ObjFunction) -> JitResult<()> {
        for param_ty in func.params_ty() {
            let abi = Self::get_abi_param(param_ty)?;
            self.ctx.func.signature.params.push(abi);
        }
        if let Some(ret) = func.return_ty() {
            let abi = Self::get_abi_param(ret)?;
            self.ctx.func.signature.params.push(abi);
        }

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let variables = declare_variables(&mut builder, func, entry);

        let mut trans = BytecodeTranslator {
            builder,
            module: &mut self.module,
            variables
        };

        trans.translate_bytecode(func)?;

        trans.builder.finalize();
        Ok(())
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
    variables: HashMap<usize, Variable>
}

impl<'a> BytecodeTranslator<'a> {
    fn translate_bytecode(&mut self, f: &ObjFunction) -> JitResult<()> {
        let ir = to_ir(f);
        Ok(())
    }

}

fn declare_variables(
    builder: &mut FunctionBuilder,
    function: &ObjFunction,
    entry_block: Block,
) -> HashMap<usize, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, ty) in function.params_ty().iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(builder, &mut variables, &mut index, ty);
        builder.def_var(var, val);
    }

    variables
}

fn declare_variable(
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<usize, Variable>,
    index: &mut usize,
    ty: &AromaType
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
}

pub type JitResult<T> = std::result::Result<T, JitError>;

#[cfg(test)]
mod tests {
    use crate::function;
    use crate::jit::JIT;
    use crate::types::Type;

    #[test]
    fn test_jit_factorial() {
        let factorial = function!(
            name "factorial",
            params (Type::Int),
            ret Type::Int,
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
}
