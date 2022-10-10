use crate::chunk::OpCode;
use crate::compiler::compile;
use crate::native::ALL_NATIVE_FNS;
use crate::value::*;
use std::collections::HashMap;
use std::rc::Rc;

pub struct VM {
    stack: Vec<Value>,
    top_frame: Frame,
    frames: Vec<Frame>,
    globals: Vec<Option<Value>>,
}

struct Frame {
    function: Rc<Function>,
    base_pointer: usize,
    ip: usize,
}

pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

// XXX should print stack trace on error
impl VM {
    pub fn interpret(source: &str) -> InterpretResult {
        let (global_ids, globals) = VM::define_natives();
        let f = match compile(source, global_ids) {
            None => return InterpretResult::CompileError,
            Some(f) => f,
        };
        let f = Rc::new(f);
        let first_frame = Frame {
            function: f.clone(),
            base_pointer: 0,
            ip: 0,
        };
        let mut vm = VM {
            stack: vec![Value::Function(f)],
            top_frame: first_frame,
            frames: Vec::new(),
            globals,
        };
        vm.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let opcode = OpCode::from_u8(self.read_byte());
            match opcode {
                OpCode::CONSTANT => {
                    let constant_index = self.read_byte();
                    let constant =
                        self.top_frame.function.chunk.constants[constant_index as usize].clone();
                    self.push(constant);
                }
                OpCode::RETURN => {
                    let ret_val = self.pop();
                    while self.stack.len() > self.top_frame.base_pointer {
                        self.pop();
                    }
                    self.push(ret_val);
                    if let Some(frame) = self.frames.pop() {
                        self.top_frame = frame;
                    } else {
                        return InterpretResult::OK;
                    }
                }
                OpCode::NEGATE => {
                    let x = self.pop();
                    self.push(negate(x));
                }
                OpCode::ADD => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(add(a, b));
                }
                OpCode::SUBTRACT => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(subtract(a, b));
                }
                OpCode::MULIPLY => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(multiply(a, b));
                }
                OpCode::DIVIDE => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(divide(a, b));
                }
                OpCode::NIL => self.push(Value::Nil),
                OpCode::TRUE => self.push(Value::Bool(true)),
                OpCode::FALSE => self.push(Value::Bool(false)),
                OpCode::NOT => {
                    let a = self.pop();
                    self.push(is_falsey(a));
                }
                OpCode::EQUAL => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.lox_eq(&b));
                }
                OpCode::GREATER => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(greater(a, b));
                }
                OpCode::LESS => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(less(a, b));
                }
                OpCode::PRINT => {
                    let a = self.pop();
                    println!("{}", a);
                }
                OpCode::POP => {
                    self.pop();
                }
                OpCode::DEF_GLOBAL => {
                    let global = self.read_byte() as usize;
                    for _ in self.globals.len()..=global {
                        self.globals.push(None);
                    }
                    let val = self.pop();
                    self.globals[global] = Some(val);
                }
                OpCode::GET_GLOBAL => {
                    let global = self.read_byte() as usize;
                    match self.globals.get(global) {
                        Some(Some(val)) => {
                            let val = val.clone();
                            self.push(val);
                        }
                        // XXX Need some kind of id -> name mapping for debugging purposes
                        _ => panic!("Undefined global variable"),
                    }
                }
                OpCode::SET_GLOBAL => {
                    let global = self.read_byte() as usize;
                    if global < self.globals.len() {
                        self.globals[global] = Some(self.peek());
                    } else {
                        panic!("Cannot assign to undeclared global variable");
                    }
                }
                OpCode::GET_LOCAL => {
                    let slot = self.read_byte() as usize;
                    self.push(self.stack[self.top_frame.base_pointer + slot].clone());
                }
                OpCode::SET_LOCAL => {
                    let slot = self.read_byte() as usize;
                    self.stack[self.top_frame.base_pointer + slot] = self.peek();
                }
                OpCode::JUMP_IF_FALSE => {
                    let offset = self.read_short() as usize;
                    if Self::is_falsey_bool(self.peek()) {
                        self.top_frame.ip += offset;
                    }
                }
                OpCode::JUMP => {
                    let offset = self.read_short() as usize;
                    self.top_frame.ip += offset;
                }
                OpCode::LOOP => {
                    let offset = self.read_short() as usize;
                    self.top_frame.ip -= offset;
                }
                OpCode::CALL => {
                    let num_args = self.read_byte() as usize;
                    let base_pointer = self.stack.len() - 1 - num_args;
                    let f = self.stack[base_pointer].clone();
                    match f {
                        Value::Function(f) => {
                            if f.arity != num_args as u8 {
                                panic!("Expected {} args but got {}", f.arity, num_args);
                            }
                            let new_frame = Frame {
                                function: f,
                                base_pointer,
                                ip: 0,
                            };
                            let old_frame = std::mem::replace(&mut self.top_frame, new_frame);
                            self.frames.push(old_frame);
                        }
                        Value::NativeFn(native) => {
                            let args = &mut self.stack[base_pointer + 1..];
                            let ret_val = (native.function)(args);
                            for _ in 0..num_args + 1 {
                                self.stack.pop();
                            }
                            self.push(ret_val);
                        }
                        _ => panic!("{f} is not a function"),
                    }
                }
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let b = self.top_frame.function.chunk.code[self.top_frame.ip];
        self.top_frame.ip += 1;
        b
    }

    fn is_falsey_bool(v: Value) -> bool {
        matches!(v, Value::Nil | Value::Bool(false))
    }

    fn read_short(&mut self) -> u16 {
        let hi_byte = self.top_frame.function.chunk.code[self.top_frame.ip] as u16;
        let lo_byte = self.top_frame.function.chunk.code[self.top_frame.ip + 1] as u16;
        self.top_frame.ip += 2;
        hi_byte << 8 | lo_byte
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("popped an empty stack")
    }

    fn peek(&self) -> Value {
        self.stack[self.stack.len() - 1].clone()
    }

    // XXX Somehow use this.
    fn define_natives() -> (HashMap<String, u8>, Vec<Option<Value>>) {
        let mut natives = HashMap::new();
        let mut globals: Vec<Option<Value>> = vec![];
        for (name, f) in ALL_NATIVE_FNS {
            natives.insert(name.to_string(), globals.len() as u8);
            let native_fn = NativeFn {
                name: name.to_string(),
                function: f,
            };
            let native_fn = Rc::new(native_fn);
            let native_fn = Value::NativeFn(native_fn);
            globals.push(Some(native_fn));
        }
        (natives, globals)
    }
}