use std::cell::{Ref, RefCell, RefMut};
use std::collections::BTreeMap;
use std::io::BufWriter;
use std::num::NonZero;
use std::ops::Neg;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicIsize, Ordering};
use std::thread;
use std::thread::JoinHandle;

use cfg_if::cfg_if;
use itertools::Itertools;
use log::{debug, trace};
use parking_lot::Mutex;

use crate::chunk::{Chunk, Constant, OpCode};
use crate::debug::Disassembler;
use crate::types::function::ObjFunction;
use crate::types::Value;
use crate::vm::{Chunks, Globals, InsPtr, StaticFunctionTable, StaticNativeTable};
use crate::vm::error::VmError;

pub type ThreadResultHolder = Arc<Mutex<Option<Result<ThreadResult, VmError>>>>;

macro_rules! binary_op {
    ($executor:expr, $op:tt?) => {
        {
            let b = $executor.pop()?;
            let a = $executor.pop()?;
            $executor.push(Value::from((a $op b)?));
        }
    };
    ($executor:expr, $op:tt) => {
        {
            let b = $executor.pop()?;
            let a = $executor.pop()?;
            $executor.push(Value::from(a $op b));
        }
    };
    ($executor:expr, $op:tt => bool) => {
        {
            let b: bool = $executor.pop()?.try_into()?;
            let a: bool = $executor.pop()?.try_into()?;
            $executor.push(Value::from(a $op b));
        }
    };
}

#[derive(Debug)]
struct StackFrame {
    function: Arc<ObjFunction>,
    stack: Vec<Value>,
    vars: BTreeMap<u8, Value>,
    pc: InsPtr,
}

/// Thread executor
#[derive(Debug)]
pub struct ThreadExecutor {
    id: AromaThreadId,
    chunks: Chunks,
    current_chunk: RefCell<Option<Arc<Chunk>>>,
    name: Option<String>,
    frame_stack: Vec<StackFrame>,
    state: ThreadState,
    globals: Globals,

    vm_run_control: Arc<AtomicIsize>,
    thread_run_control: Arc<AtomicIsize>,
    static_functions: StaticFunctionTable,
    native_functions: StaticNativeTable,
}

impl ThreadExecutor {
    pub fn start(
        calling_function: Arc<ObjFunction>,
        id: AromaThreadId,
        chunks: Chunks,
        globals: Globals,
        ip: InsPtr,
        vm_run_control: Arc<AtomicIsize>,
        result_mutex: ThreadResultHolder,
        static_functions: StaticFunctionTable,
        native_functions: StaticNativeTable,
    ) -> AromaThreadHandle {
        let thread_controller = Arc::new(AtomicIsize::new(0));
        let handle = {
            let thread_controller = thread_controller.clone();
            thread::spawn(move || {
                let mut this = Self {
                    id,
                    chunks,
                    current_chunk: RefCell::new(None),
                    name: None,
                    frame_stack: vec![],
                    state: ThreadState::Dead,
                    globals,
                    vm_run_control,
                    thread_run_control: thread_controller,
                    static_functions,
                    native_functions,
                };

                this.frame_stack.push(StackFrame {
                    function: calling_function,
                    stack: vec![],
                    vars: Default::default(),
                    pc: ip,
                });
                let ret = this.run();
                if let Err(e) = &ret {
                    eprintln!("{}", e);
                    eprintln!("{:#?}", this.frame_stack);
                }

                *result_mutex.lock() = Some(ret);
            })
        };
        AromaThreadHandle {
            id,
            handle,
            thread_run_control: thread_controller,
        }
    }

    fn run(&mut self) -> Result<ThreadResult, VmError> {
        while !self.frame_stack.is_empty() {
            // only execute while thread control == 0
            if self.vm_run_control.load(Ordering::Relaxed) > 0
                || self.thread_run_control.load(Ordering::Relaxed) > 0
            {
                thread::yield_now();
                continue;
            }

            let frame = self.current_frame();
            #[cfg(feature = "debug_trace_execution")]
            {
                trace!(
                    "stack: {}",
                    frame
                        .stack
                        .iter()
                        .fold(String::new(), |accum, next| format!("{accum}[{next}]"))
                );
                trace!(
                    "vars: {}",
                    frame
                        .vars
                        .iter()
                        .map(|(idx, val)| format!(
                            "var_{idx}={val}"
                        ))
                        .join(", ")
                );
            }

            let instruction = self.read_byte()?;
            let op_code = OpCode::try_from(instruction)?;
            #[cfg(feature = "debug_trace_execution")]
            {
                trace!("{}", op_code.as_ref());
                // trace!("frame stack: {:#?}", &self.frame_stack);
                trace!("");
            }


            match op_code {
                OpCode::Constant => {
                    let constant = self.read_constant()?;
                    self.stack_mut().push(constant);
                }
                OpCode::Return => {
                    let last = self.pop().ok();
                    let last_frame = self.frame_stack.pop();
                    #[cfg(feature = "debug_trace_execution")]
                    {
                        if let Some(last_frame) = last_frame {
                            if let Some(last) = &last {
                                debug!(
                                    "{}function {:?} returned {last}",
                                    ".".repeat(self.frame_stack.len()),
                                    last_frame.function.name()
                                )
                            } else {
                                debug!(
                                    "{}function {:?} returned",
                                    ".".repeat(self.frame_stack.len()),
                                    last_frame.function.name()
                                )
                            }
                        }
                    }
                    if !self.frame_stack.is_empty() {
                        if let Some(last) = last {
                            self.push(last);
                        }
                    } else {
                        if let Some(Value::Int(i)) = last {
                            return Ok(ThreadResult::Done(i));
                        }
                    }
                }
                OpCode::Negate => {
                    let neg = (-self.pop()?)?;
                    self.push(neg);
                }
                OpCode::Add => {
                    binary_op!(self, +?)
                }
                OpCode::Subtract => {
                    binary_op!(self, -?)
                }
                OpCode::Mult => {
                    binary_op!(self, *?)
                }
                OpCode::Divide => {
                    binary_op!(self, /?)
                }
                OpCode::Eq => {
                    binary_op!(self, ==)
                }
                OpCode::Neq => {
                    binary_op!(self, !=)
                }
                OpCode::Gt => {
                    binary_op!(self, >)
                }
                OpCode::Gte => {
                    binary_op!(self, >=)
                }
                OpCode::Lt => {
                    binary_op!(self, <)
                }
                OpCode::Lte => {
                    binary_op!(self, <=)
                }
                OpCode::And => {
                    binary_op!(self, && => bool)
                }
                OpCode::Not => {}
                OpCode::Or => {
                    binary_op!(self, || => bool)
                }
                OpCode::Pop => {
                    let _ = self.pop()?;
                }

                OpCode::SetLocalVar => {
                    let val = self.pop()?;
                    let var_idx = self.read_byte()?;
                    self.current_frame_mut().vars.insert(var_idx, val);
                }
                OpCode::GetLocalVar => {
                    let var_idx = self.read_byte()?;
                    let val = self
                        .current_frame()
                        .vars
                        .get(&var_idx)
                        .ok_or_else(|| VmError::NoLocalError(var_idx))?
                        .clone();
                    self.push(val);
                }
                OpCode::GetGlobalVar => {
                    let var_global_idx: String = self.read_constant()?.try_into()?;
                    let var = self
                        .globals
                        .read()
                        .get(&var_global_idx)
                        .cloned()
                        .ok_or_else(|| VmError::NoGlobal(var_global_idx))?;
                    self.push(var);
                }
                OpCode::SetGlobalVar => {
                    let var_global_idx: String = self.read_constant()?.try_into()?;
                    let val = self.pop()?;
                    self.globals.write().insert(var_global_idx, val);
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short()?;
                    let check: bool = *self.peek()?.as_bool().ok_or(VmError::BooleanExpected)?;

                    if !check {
                        self.add_offset(usize::from(offset) as isize);
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short()?;
                    self.add_offset(usize::from(offset) as isize);
                }
                OpCode::Loop => {
                    let offset = self.read_short()?;
                    let offset = -i32::from(offset);
                    if offset > 0 {
                        panic!("loop can never go forward")
                    }
                    self.add_offset(offset as isize);
                }
                OpCode::Call => {
                    let argc = self.read_byte()?;
                    let value = self.pop()?;
                    self.call_value(value, argc as usize)?;
                }
                OpCode::LtoI => {
                    let p = self.pop()?;
                    let Value::Long(long) = p else {
                        return Err(VmError::TypeError(p, "Long".to_string()));
                    };
                    self.push(Value::Int(long as i32))
                }
                OpCode::IToL => {
                    let p = self.pop()?;
                    let Value::Int(long) = p else {
                        return Err(VmError::TypeError(p, "Int".to_string()));
                    };
                    self.push(Value::Long(long as i64))
                }
                OpCode::Closure => {}
            }
        }

        Ok(ThreadResult::Done(0))
    }

    fn call_value(&mut self, callee: Value, argc: usize) -> Result<(), VmError> {
        match callee {
            Value::Function(f) => {
                self.call(f, argc)?;
            }
            Value::Native(native) => {
                let mut stack = vec![];
                for _ in 0..argc {
                    stack.push(self.pop()?);
                }
                stack.reverse();
                debug!("calling native function {:?}", native.name());
                let ret = native.native()(&stack)?;
                if let Some(ret) = ret {
                    self.push(ret);
                }
            }
            _ => return Err(VmError::IllegalOperation("call".to_string(), callee)),
        }

        Ok(())
    }

    fn call(&mut self, function: Arc<ObjFunction>, argc: usize) -> Result<(), VmError> {
        let mut stack = vec![];
        for _ in 0..argc {
            stack.push(self.pop()?);
        }
        stack.reverse();

        #[cfg(feature = "debug_trace_execution")]
        {
            debug!(
                "{}calling {:?} with {:?}",
                ".".repeat(self.frame_stack.len()),
                function.name(),
                stack
            );
        }
        let chunk_idx = function
            .chunk_idx()
            .ok_or_else(|| VmError::FunctionNotLoaded(function.name().to_string()))?;
        function.mark_executed();

        cfg_if! {
            if #[cfg(feature="jit")] {
                if let Some(jit) = function.jit() {
                    #[cfg(feature = "debug_trace_execution")] {
                        trace!("calling compiled {} at {:p}", function.name(), jit);
                    }
                    if let Some(ret_value) = crate::jit::abi::call_jit(&*stack, &*function, jit) {
                        self.push(ret_value);
                    }
                } else {
                    self.frame_stack.push(StackFrame {
                        function,
                        stack,
                        vars: Default::default(),
                        pc: (chunk_idx, 0),
                    });
                    if chunk_idx != self.frame_stack[self.frame_stack.len() - 2].pc.0 {
                        let mut binding = self.current_chunk.borrow_mut();
                        let _ = binding.take();
                    }
                }
            } else {
                self.frame_stack.push(StackFrame {
                        function,
                        stack,
                        vars: Default::default(),
                        pc: (chunk_idx, 0),
                    });
                    if chunk_idx != self.frame_stack[self.frame_stack.len() - 2].pc.0 {
                        let mut binding = self.current_chunk.borrow_mut();
                        let _ = binding.take();
                    }
            }
        }

        Ok(())
    }

    fn current_frame(&self) -> &StackFrame {
        self.frame_stack
            .last()
            .expect("Threads conclude when no stack frames are left")
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.frame_stack
            .last_mut()
            .expect("Threads conclude when no stack frames are left")
    }

    fn stack(&self) -> &Vec<Value> {
        &self.current_frame().stack
    }

    fn stack_mut(&mut self) -> &mut Vec<Value> {
        &mut self.current_frame_mut().stack
    }

    fn push(&mut self, value: Value) {
        self.stack_mut().push(value);
    }

    fn pop(&mut self) -> Result<Value, VmError> {
        self.stack_mut().pop().ok_or(VmError::NoValueOnStack)
    }

    fn peek(&self) -> Result<&Value, VmError> {
        self.stack().last().ok_or(VmError::NoValueOnStack)
    }

    /// Add offset to the current ip, changing to the next chunk if necessary
    fn add_offset(&mut self, offset: isize) {
        let (chunk_p, ip) = self.current_frame().pc;
        let chunk = self.current_frame_chunk();
        let next_ip: (usize, usize);
        let next_offset = ip.wrapping_add_signed(offset);
        if chunk.len() == next_offset {
            next_ip = (chunk_p + 1, 0);
        } else {
            next_ip = (chunk_p, next_offset);
        }
        drop(chunk);
        if self.current_frame().pc.0 != next_ip.0 {
            let _ = self.current_chunk.borrow_mut().take();
        }
        self.current_frame_mut().pc = next_ip;
    }

    /// Read a byte from the chunks based on the IP
    fn read_byte(&mut self) -> Result<u8, VmError> {
        let (_, ip) = self.current_frame().pc;
        let chunk = self.current_frame_chunk();
        let out = chunk
            .code()
            .get(ip)
            .copied()
            .ok_or_else(|| VmError::NoInstruction);
        drop(chunk);
        self.add_offset(1);
        out
    }

    /// Read an u16 from the chunks based on the IP
    fn read_short(&mut self) -> Result<u16, VmError> {
        let (_, ip) = self.current_frame().pc;
        let chunk = self.current_frame_chunk();
        let out = chunk
            .code()
            .get(ip..)
            .map(|slice| &slice[..2])
            .map(|bytes| {
                <u16>::from_be_bytes(<[u8; 2]>::try_from(bytes).expect("expected two bytes"))
            })
            .ok_or_else(|| VmError::NoInstruction);
        drop(chunk);
        self.add_offset(2);
        out
    }

    fn read_constant(&mut self) -> Result<Value, VmError> {
        let get_constant = {
            let chunk = Vec::from(self.current_frame_chunk().constants());
            move |idx: u8| chunk[idx as usize]
        };

        let constant_idx = self.read_byte()?;
        let mut constant: Constant = get_constant(constant_idx);
        match constant {
            Constant::FunctionId(id) => match get_constant(id) {
                Constant::Utf8(utf8) => {
                    return if let Some(function) = self.static_functions.read().get(utf8).cloned() {
                        Ok(Value::Function(function))
                    } else if let Some(native) = self.native_functions.read().get(utf8).cloned() {
                        Ok(Value::Native(native))
                    } else {
                        Err(VmError::FunctionNotDefined(utf8.to_string()))
                    }
                }
                other => Err(VmError::UnexpectedConstant(other)),
            },
            Constant::Int(i) => Ok(Value::Int(i)),
            Constant::Long(i) => Ok(Value::Long(i)),
            Constant::String(s) => match get_constant(s) {
                Constant::Utf8(utf8) => Ok(Value::String(utf8.to_string())),
                other => Err(VmError::UnexpectedConstant(other)),
            },
            other => Err(VmError::UnexpectedConstant(other)),
        }
    }

    // provides a
    fn current_frame_chunk(&self) -> Arc<Chunk> {
        // self.chunks.read()[self.current_frame().pc.0].clone()
        let pc = self.current_frame().pc.0;
        {
            let mut binding = self.current_chunk.borrow_mut();
            if binding.is_none() {
                let guard = self.chunks.read();
                let _ = binding.insert(guard[pc].clone());
            }
        }
        let borrowed = self.current_chunk.borrow();
        borrowed.as_ref().unwrap().clone()
    }
}

#[derive(Debug)]
pub struct AromaThreadHandle {
    id: AromaThreadId,
    handle: JoinHandle<()>,
    thread_run_control: Arc<AtomicIsize>,
}

impl AromaThreadHandle {
    pub fn id(&self) -> &AromaThreadId {
        &self.id
    }

    pub fn join(self) -> Result<(), VmError> {
        self.thread_run_control.store(-1, Ordering::SeqCst);
        self.handle.join().map_err(|e| {
            VmError::ThreadPanicked(e.downcast_ref::<&str>().map(|s| s.to_string()))
        })?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ThreadResult {
    Done(i32), // if any non-zero code returned, exit.
    Exception(Value),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ThreadState {
    Dead,
    Running,
    Waiting,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct AromaThreadId(NonZero<usize>);

impl AromaThreadId {
    pub fn new(id: NonZero<usize>) -> Self {
        Self(id)
    }
}
