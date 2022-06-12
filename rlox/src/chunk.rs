use crate::value::Value;
use std::convert::TryFrom;
use std::convert::TryInto;

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

    pub fn write(&mut self, b: u8, line: usize) {
        self.code.push(b);
        self.lines.push(line);
    }

    pub fn write_constant(&mut self, v: Value, line: usize) {
        let index = self.constants.len();
        self.constants.push(v);
        self.write(OpCode::CONSTANT as u8, line);
        self.write(index as u8, line);
    }
}

pub struct ChunkIter<'a> {
    byte_iter: std::slice::Iter<'a, u8>,
    chunk: &'a Chunk,
}