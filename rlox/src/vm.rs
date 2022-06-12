use crate::chunk::OpCode;
use crate::compiler::compile;
use crate::Chunk;
use crate::Value;

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

fn negate(v: Value) -> Value {
    match v {
        Value::Num(x) => Value::Num(-x),
    }
}

fn add(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
    }
}

fn subtract(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
    }
}

fn multiply(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
    }
}

fn divide(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
    }
}
