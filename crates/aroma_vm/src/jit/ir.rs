use crate::chunk::{Chunk, Constant, OpCode};
use crate::jit::{JitError, JitResult};
use crate::types::function::ObjFunction;
use crate::types::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    CallFunction(String, Vec<Box<Op>>),
    Param(usize),
    Constant(Value),
    Function(String),
    Return(Box<Op>),
    Assign(usize, Box<Op>),
    BinOp(OpCode, Box<Op>, Box<Op>),
    UniOp(OpCode, Box<Op>),
    Call(Box<Op>, Vec<Op>),
    GetLocal(usize),
    If {
        cond: Box<Op>,
        then: Vec<Op>,
        otherwise: Vec<Op>,
    },
}

/// Converts some byte code to some ir
pub fn to_ir(f: &ObjFunction) -> JitResult<Vec<Op>> {
    let mut ops = vec![];
    for i in (0..f.arity()).rev() {
        ops.push(Op::Param(i));
    }

    for chunk in f.chunks() {
        chunk_to_ir(0, chunk, &mut ops, f)?;
    }

    Ok(ops)
}

fn chunk_to_ir(
    start_idx: usize,
    chunk: &Chunk,
    ops: &mut Vec<Op>,
    owning_func: &ObjFunction,
) -> JitResult<()> {
    let mut offset = start_idx;
    debug_assert!(
        offset < chunk.len(),
        "offset {offset} > chunk.len() {}",
        chunk.len()
    );
    while offset < chunk.len() {
        let instruction = OpCode::try_from(chunk.code()[offset])?;
        match instruction {
            OpCode::JumpIfFalse => {
                let cond = ops.last().unwrap().clone();
                let mut if_false = ops.clone();

                let jump_offset =
                    i16::from_be_bytes(chunk.code()[(offset + 1)..][..2].try_into().unwrap());
                let mut fall_through = ops.clone();
                chunk_to_ir(offset + 3, chunk, &mut fall_through, owning_func)?;
                chunk_to_ir(
                    (offset + 3).saturating_add_signed(jump_offset as isize),
                    chunk,
                    &mut if_false,
                    owning_func,
                )?;

                let remove = ops.len().saturating_sub(1);

                if_false.drain(..remove);
                fall_through.drain(..remove);

                let after = if let Some(sub) = find_debranch_rev_index(&if_false, &fall_through) {
                    if_false.truncate(if_false.len().saturating_sub(sub));
                    let final_len = fall_through.len().saturating_sub(sub);
                    fall_through.drain(final_len..).collect::<Vec<_>>()
                } else {
                    vec![]
                };

                ops.pop().unwrap();
                let branch = Op::If {
                    cond: Box::new(cond),
                    then: fall_through,
                    otherwise: if_false,
                };

                ops.push(branch);
                ops.extend(after);

                return Ok(());
            }
            OpCode::Jump => {
                panic!("jump");
            }
            OpCode::Call => {
                let function = ops.pop().expect("call must have an op");
                let arg_c = chunk.code()[offset + 1] as usize;
                let final_len = ops.len().saturating_sub(arg_c);
                let args = ops.drain(final_len..).collect::<Vec<_>>();

                ops.push(Op::Call(Box::new(function), args));
            }
            other => {
                if is_operand(other) {
                    match other {
                        OpCode::Constant => {
                            // constant values
                            let c_index = chunk.code()[offset + 1];
                            let constant = chunk
                                .get_constant(c_index)
                                .ok_or(JitError::NoConstant(c_index))?;
                            let op = match constant {
                                &Constant::Int(i) => Op::Constant(Value::Int(i)),
                                &Constant::Long(l) => Op::Constant(Value::Long(l)),
                                &Constant::String(s_idx) => {
                                    let c = chunk
                                        .get_constant(s_idx)
                                        .ok_or(JitError::NoConstant(c_index))?;
                                    let Constant::Utf8(str) = c else {
                                        panic!("Strings should always have a reference to a utf8 constant: {c:?}")
                                    };
                                    Op::Constant(Value::String(str.to_string()))
                                }
                                &Constant::FunctionId(s_idx) => {
                                    let c = chunk
                                        .get_constant(s_idx)
                                        .ok_or(JitError::NoConstant(c_index))?;
                                    let Constant::Utf8(str) = c else {
                                        panic!("Strings should always have a reference to a utf8 constant: {c:?}")
                                    };
                                    Op::Function(str.to_string())
                                }
                                other => {
                                    return Err(JitError::UnrepresentableConstant(other.clone()))
                                }
                            };
                            ops.push(op);
                        }
                        OpCode::GetLocalVar => {
                            let var_idx = chunk.code()[offset + 1];
                            ops.push(Op::GetLocal(var_idx as usize));
                        }
                        _ => unreachable!(),
                    }
                } else {
                    let op_c = operands(instruction);
                    let final_len = ops.len().saturating_sub(op_c);
                    let mut operands = ops.drain(final_len..).collect::<Vec<_>>();
                    match other {
                        OpCode::Return => {
                            let a = operands.pop().unwrap();
                            ops.push(Op::Return(Box::new(a)));
                            break;
                        }
                        OpCode::Negate => {}
                        OpCode::Add
                        | OpCode::Subtract
                        | OpCode::Mult
                        | OpCode::Divide
                        | OpCode::Eq
                        | OpCode::Neq
                        | OpCode::Gt
                        | OpCode::Gte
                        | OpCode::Lt
                        | OpCode::Lte
                        | OpCode::And
                        | OpCode::Not
                        | OpCode::Or => {
                            let b = operands.pop().unwrap();
                            let a = operands.pop().unwrap();
                            ops.push(Op::BinOp(instruction, Box::new(a), Box::new(b)));
                        }
                        OpCode::Pop => {
                            operands.pop().unwrap();
                        }
                        OpCode::LtoI => {}
                        OpCode::IToL => {}
                        OpCode::SetLocalVar => {
                            let var_idx = chunk.code()[offset + 1];
                            let v = operands.pop().unwrap();
                            ops.push(Op::Assign(var_idx as usize, Box::new(v)));
                        }
                        OpCode::SetGlobalVar => {}
                        _other => unreachable!(),
                    }

                    if !operands.is_empty() {
                        panic!("not all operands used for {other} but {op_c} were requested")
                    }
                }
            }
        }
        offset += bytes(instruction);
    }
    Ok(())
}

#[inline]
fn is_operand(op_code: OpCode) -> bool {
    operands(op_code) == 0
}
fn operands(op_code: OpCode) -> usize {
    match op_code {
        OpCode::Constant => 0,
        OpCode::Return => 1,
        OpCode::Negate => 1,
        OpCode::Not => 1,
        OpCode::Pop => 1,
        OpCode::LtoI => 1,
        OpCode::IToL => 1,
        OpCode::Add => 2,
        OpCode::Subtract => 2,
        OpCode::Mult => 2,
        OpCode::Divide => 2,
        OpCode::Eq => 2,
        OpCode::Neq => 2,
        OpCode::Gt => 2,
        OpCode::Gte => 2,
        OpCode::Lt => 2,
        OpCode::Lte => 2,
        OpCode::And => 2,
        OpCode::Or => 2,

        OpCode::SetLocalVar => 1,
        OpCode::GetLocalVar => 0,
        OpCode::GetGlobalVar => 0,
        OpCode::SetGlobalVar => 1,
        OpCode::Jump => 0,
        OpCode::Call | OpCode::JumpIfFalse => {
            panic!("Has dynamic inputs")
        }
    }
}

fn bytes(op_code: OpCode) -> usize {
    match op_code {
        OpCode::Constant
        | OpCode::SetGlobalVar
        | OpCode::GetGlobalVar
        | OpCode::SetLocalVar
        | OpCode::GetLocalVar
        | OpCode::Call => 2,
        OpCode::Jump | OpCode::JumpIfFalse => 3,
        _default => 1,
    }
}

fn find_debranch_rev_index(a: &Vec<Op>, b: &Vec<Op>) -> Option<usize> {
    let mut ret = None;
    for (idx, (a_op, b_op)) in a.iter().rev().zip(b.iter().rev()).enumerate() {
        if a_op == b_op {
            let _ = ret.insert(idx);
        } else {
            break;
        }
    }
    ret
}

#[cfg(test)]
mod tests {
    use Op::*;

    use crate::examples::fibonacci;

    use super::*;

    #[test]
    fn test_function_to_ir() {
        let func = fibonacci();
        let ir = to_ir(&func).expect("should be able to create IR");

        let expected = vec![
            Assign(0, Box::new(Param(0))),
            If {
                cond: Box::new(BinOp(
                    OpCode::Lt,
                    Box::new(GetLocal(0)),
                    Box::new(Constant(Value::Long(2))),
                )),
                then: vec![Return(Box::new(GetLocal(0)))],
                otherwise: vec![Return(Box::new(BinOp(
                    OpCode::Add,
                    Box::new(Call(
                        Box::new(Function("fibonacci".to_string())),
                        vec![BinOp(
                            OpCode::Subtract,
                            Box::new(GetLocal(0)),
                            Box::new(Constant(Value::from(1_i64))),
                        )],
                    )),
                    Box::new(Call(Box::new(Function("fibonacci".to_string())), vec![
                        BinOp(
                            OpCode::Subtract,
                            Box::new(GetLocal(0)),
                            Box::new(Constant(Value::from(2_i64))),
                        )
                    ])),
                )))],
            },
        ];
        assert_eq!(ir, expected);
    }
}
