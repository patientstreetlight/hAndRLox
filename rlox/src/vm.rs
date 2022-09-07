use crate::chunk::OpCode;
use crate::compiler::compile;
use crate::Chunk;
use crate::value::*;

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
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
        };
        return vm.run();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let opcode = OpCode::from_u8(self.read_byte());
            match opcode {
                OpCode::CONSTANT => {
                    let constant_index = self.read_byte();
                    let constant = self.chunk.constants[constant_index as usize];
                    self.push(constant);
                }
                OpCode::RETURN => {
                    let val = self.pop();
                    println!("{:?}", val);
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
}
