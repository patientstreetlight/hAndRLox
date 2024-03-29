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
    NIL,
    TRUE,
    FALSE,
    NOT,
    EQUAL,
    GREATER,
    LESS,
    PRINT,
    POP,
    DEF_GLOBAL,
    GET_GLOBAL,
    SET_GLOBAL,
    GET_LOCAL,
    SET_LOCAL,
    JUMP_IF_FALSE,
    JUMP,
    LOOP,
    CALL,
    CLOSURE,
    GET_UPVALUE,
    SET_UPVALUE,
    CLOSE_UPVALUE,
    CLASS,
    SET_PROPERTY,
    GET_PROPERTY,
    METHOD,
    INVOKE,
    INHERIT,
    GET_SUPER,
    SUPER_INVOKE,
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
            x if x == OpCode::NIL as u8 => Ok(OpCode::NIL),
            x if x == OpCode::TRUE as u8 => Ok(OpCode::TRUE),
            x if x == OpCode::FALSE as u8 => Ok(OpCode::FALSE),
            x if x == OpCode::NOT as u8 => Ok(OpCode::NOT),
            x if x == OpCode::EQUAL as u8 => Ok(OpCode::EQUAL),
            x if x == OpCode::GREATER as u8 => Ok(OpCode::GREATER),
            x if x == OpCode::LESS as u8 => Ok(OpCode::LESS),
            x if x == OpCode::PRINT as u8 => Ok(OpCode::PRINT),
            x if x == OpCode::POP as u8 => Ok(OpCode::POP),
            x if x == OpCode::DEF_GLOBAL as u8 => Ok(OpCode::DEF_GLOBAL),
            x if x == OpCode::GET_GLOBAL as u8 => Ok(OpCode::GET_GLOBAL),
            x if x == OpCode::SET_GLOBAL as u8 => Ok(OpCode::SET_GLOBAL),
            x if x == OpCode::GET_LOCAL as u8 => Ok(OpCode::GET_LOCAL),
            x if x == OpCode::SET_LOCAL as u8 => Ok(OpCode::SET_LOCAL),
            x if x == OpCode::JUMP_IF_FALSE as u8 => Ok(OpCode::JUMP_IF_FALSE),
            x if x == OpCode::JUMP as u8 => Ok(OpCode::JUMP),
            x if x == OpCode::LOOP as u8 => Ok(OpCode::LOOP),
            x if x == OpCode::CALL as u8 => Ok(OpCode::CALL),
            x if x == OpCode::CLOSURE as u8 => Ok(OpCode::CLOSURE),
            x if x == OpCode::GET_UPVALUE as u8 => Ok(OpCode::GET_UPVALUE),
            x if x == OpCode::SET_UPVALUE as u8 => Ok(OpCode::SET_UPVALUE),
            x if x == OpCode::CLOSE_UPVALUE as u8 => Ok(OpCode::CLOSE_UPVALUE),
            x if x == OpCode::CLASS as u8 => Ok(OpCode::CLASS),
            x if x == OpCode::GET_PROPERTY as u8 => Ok(OpCode::GET_PROPERTY),
            x if x == OpCode::SET_PROPERTY as u8 => Ok(OpCode::SET_PROPERTY),
            x if x == OpCode::METHOD as u8 => Ok(OpCode::METHOD),
            x if x == OpCode::INVOKE as u8 => Ok(OpCode::INVOKE),
            x if x == OpCode::INHERIT as u8 => Ok(OpCode::INHERIT),
            x if x == OpCode::GET_SUPER as u8 => Ok(OpCode::GET_SUPER),
            x if x == OpCode::SUPER_INVOKE as u8 => Ok(OpCode::SUPER_INVOKE),
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

    pub fn mk_constant(&mut self, v: Value, line: usize) -> usize {
        let index = self.constants.len();
        self.constants.push(v);
        index
    }
}
