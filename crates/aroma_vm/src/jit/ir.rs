pub use ir_builder::{Block, IrBlock, IrFunction};
pub use ir_compiler::{CompileIrError, CompileIrResult, IrCompiler};
pub use ir_op::{IrOp, IrValue};

mod ir_builder;
mod ir_compiler;
mod ir_op;


#[cfg(test)]
mod tests {
    use crate::jit::ir::ir_op::IrOp::*;
}
