use crate::chunk::{Chunk, OpCode};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;

pub fn compile(source: &str, chunk: &mut Chunk) -> bool {
    let mut parser = Parser::new(source, chunk);
    parser.expression();
    parser.consume(TokenType::EOF, "Expect end of expression.");
    parser.end_compiler();
    !parser.had_error
}

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
    scanner: Scanner<'a>,
    had_error: bool,
    panic_mode: bool,
    chunk: &'a mut Chunk,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next_highest(&self) -> Precedence {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => panic!("No precedence higher than Primary"),
        }
    }
}

struct BinaryOp {
    associativity: Associativity,
    precedence: Precedence,
    opcode: OpCode,
}

enum Associativity {
    Left,
    Right,
}

fn get_binary_op(token_type: TokenType) -> Option<BinaryOp> {
    match token_type {
        TokenType::Minus => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Term,
            opcode: OpCode::SUBTRACT,
        }),
        TokenType::Plus => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Term,
            opcode: OpCode::ADD,
        }),
        TokenType::Star => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Factor,
            opcode: OpCode::MULIPLY,
        }),
        TokenType::Slash => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Factor,
            opcode: OpCode::DIVIDE,
        }),
        _ => None,
    }
}

fn get_prefix_parser<'a>(token_type: TokenType) -> Option<fn(&mut Parser<'a>)> {
    match token_type {
        TokenType::LeftParen => Some(Parser::grouping),
        TokenType::Minus => Some(Parser::unary),
        TokenType::Number => Some(Parser::number),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, chunk: &'a mut Chunk) -> Parser<'a> {
        let mut scanner = Scanner::new(source);
        let first_token = scanner.scan_token();
        Parser {
            previous: first_token,
            current: first_token,
            scanner,
            had_error: false,
            panic_mode: false,
            chunk,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;
        loop {
            self.current = self.scanner.scan_token();
            if self.current.token_type != TokenType::Error {
                break;
            }
            self.error_at_current(self.current.lexeme);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn consume(&mut self, expected: TokenType, err_msg: &str) {
        if self.current.token_type == expected {
            self.advance();
        } else {
            self.error_at_current(err_msg);
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current, msg);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous, msg);
    }

    fn error_at(&mut self, token: Token, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        let err_body = match token.token_type {
            TokenType::EOF => String::from(" at end"),
            TokenType::Error => String::from(""),
            _ => format!(" at '{}'", token.lexeme),
        };
        eprintln!("[line {}] Error{}: {}", token.line, &err_body, msg);
        self.had_error = true;
    }

    fn emit_byte(&mut self, b: u8) {
        self.chunk.write(b, self.previous.line as usize);
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8) {
        self.emit_byte(b1);
        self.emit_byte(b2);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::RETURN as u8);
    }

    fn number(&mut self) {
        let n: f64 = self.previous.lexeme.parse().unwrap();
        self.emit_constant(Value::Num(n));
    }

    fn emit_constant(&mut self, val: Value) {
        self.chunk.write_constant(val, self.previous.line as usize);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_type = self.previous.token_type;
        self.parse_precedence(Precedence::Unary);
        let op = match operator_type {
            TokenType::Minus => OpCode::NEGATE,
            _ => panic!("bad unary operator type"),
        };
        self.emit_byte(op as u8);
    }

    fn binary(&mut self, binary_op: BinaryOp) {
        // parse right hand side
        let rhs_precedence = match binary_op.associativity {
            Associativity::Left => binary_op.precedence.next_highest(),
            Associativity::Right => binary_op.precedence,
        };
        self.parse_precedence(rhs_precedence);
        self.emit_byte(binary_op.opcode as u8);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        match get_prefix_parser(self.previous.token_type) {
            Some(prefix_rule) => prefix_rule(self),
            None => {
                self.error("Expect expression.");
                return;
            }
        }
        loop {
            match get_binary_op(self.current.token_type) {
                Some(binary_op) if binary_op.precedence >= precedence => {
                    self.advance();
                    self.binary(binary_op);
                }
                _ => break,
            }
        }
    }
}
