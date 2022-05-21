use crate::value::Value;
use std::convert::TryFrom;
use std::convert::TryInto;

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    CONSTANT(Value),
    NEGATE,
    ADD,
    SUBTRACT,
    MULIPLY,
    DIVIDE,
    RETURN,
}

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    CONSTANT,
    NEGATE,
    ADD,
    SUBTRACT,
    MULIPLY,
    DIVIDE,
    RETURN,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == OpCode::CONSTANT as u8 => Ok(OpCode::CONSTANT),
            x if x == OpCode::NEGATE as u8 => Ok(OpCode::NEGATE),
            x if x == OpCode::RETURN as u8 => Ok(OpCode::RETURN),
            x if x == OpCode::ADD as u8 => Ok(OpCode::ADD),
            x if x == OpCode::SUBTRACT as u8 => Ok(OpCode::SUBTRACT),
            x if x == OpCode::MULIPLY as u8 => Ok(OpCode::MULIPLY),
            x if x == OpCode::DIVIDE as u8 => Ok(OpCode::DIVIDE),
            _ => Err(()),
        }
    }
}

impl OpCode {
    pub fn from_u8(b: u8) -> OpCode {
        match b.try_into() {
            Ok(opcode) => opcode,
            Err(_) => panic!("invalid opcode"),
        }
    }
}

impl Instruction {
    fn opcode(&self) -> OpCode {
        match self {
            Self::CONSTANT(_) => OpCode::CONSTANT,
            Self::NEGATE => OpCode::NEGATE,
            Self::RETURN => OpCode::RETURN,
            Self::ADD => OpCode::ADD,
            Self::SUBTRACT => OpCode::SUBTRACT,
            Self::MULIPLY => OpCode::MULIPLY,
            Self::DIVIDE => OpCode::DIVIDE,
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, inst: Instruction, line: usize) {
        self.code.push(inst.opcode() as u8);
        self.lines.push(line);
        match inst {
            Instruction::CONSTANT(v) => {
                let index = self.constants.len();
                self.constants.push(v);
                self.code.push(index as u8);
                self.lines.push(line);
            }
            _ => ()
        }
    }

    pub fn instructions(&self) -> ChunkIter {
        ChunkIter {
            byte_iter: self.code.iter(),
            chunk: self,
        }
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        for i in self.instructions() {
            println!("{:?}", i);
        }
    }
}

pub struct ChunkIter<'a> {
    byte_iter: std::slice::Iter<'a, u8>,
    chunk: &'a Chunk,
}

impl Iterator for ChunkIter<'_> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        let byte = self.byte_iter.next()?;
        match byte {
            0 => {
                let constant_index = *self.byte_iter.next().expect("Expected constant index");
                let constant = self.chunk.constants[constant_index as usize];
                Some(Instruction::CONSTANT(constant))
            }
            1 => {
                Some(Instruction::RETURN)
            }
            _ => panic!("unknown opcode")
        }
    }
}