use crate::chunk::OpCode;
use crate::compiler::compile;
use crate::value::*;
use crate::Chunk;

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    globals: Vec<Option<Value>>,
}

pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

impl VM {
    pub fn interpret(source: &str) -> InterpretResult {
        let mut chunk = Chunk::new();
        if !compile(source, &mut chunk) {
            return InterpretResult::CompileError;
        }
        let mut vm = VM {
            chunk: chunk,
            ip: 0,
            stack: Vec::new(),
            globals: Vec::new(),
        };
        return vm.run();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let opcode = OpCode::from_u8(self.read_byte());
            match opcode {
                OpCode::CONSTANT => {
                    let constant_index = self.read_byte();
                    let constant = self.chunk.constants[constant_index as usize].clone();
                    self.push(constant);
                }
                OpCode::RETURN => {
                    return InterpretResult::OK;
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
                    for i in self.globals.len()..=global {
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
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let b = self.chunk.code[self.ip];
        self.ip += 1;
        b
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
}
