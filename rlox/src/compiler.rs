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

struct BinaryOp<'a> {
    associativity: Associativity,
    precedence: Precedence,
    parser: fn(&mut Parser<'a>),
}

enum Associativity {
    Left,
    Right,
}

fn get_infix_parser<'a>(token_type: TokenType) -> Option<BinaryOp<'a>> {
    match token_type {
        TokenType::Minus => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Term,
            parser: Parser::binary,
        }),
        TokenType::Plus => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Term,
            parser: Parser::binary,
        }),
        TokenType::Star => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Factor,
            parser: Parser::binary,
        }),
        TokenType::Slash => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Factor,
            parser: Parser::binary,
        }),
        TokenType::BangEqual => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Equality,
            parser: Parser::binary,
        }),
        TokenType::EqualEqual => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Equality,
            parser: Parser::binary,
        }),
        TokenType::Greater => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        TokenType::GreaterEqual => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        TokenType::Less => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        TokenType::LessEqual => Some(BinaryOp {
            associativity: Associativity::Left,
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        _ => None,
    }
}

fn get_prefix_parser<'a>(token_type: TokenType) -> Option<fn(&mut Parser<'a>)> {
    match token_type {
        TokenType::LeftParen => Some(Parser::grouping),
        TokenType::Minus => Some(Parser::unary),
        TokenType::Number => Some(Parser::number),
        TokenType::True => Some(Parser::literal),
        TokenType::False => Some(Parser::literal),
        TokenType::Nil => Some(Parser::literal),
        TokenType::Bang => Some(Parser::unary),
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
            TokenType::Bang => OpCode::NOT,
            _ => panic!("bad unary operator type"),
        };
        self.emit_byte(op as u8);
    }

    fn binary(&mut self) {
        let token_type = self.previous.token_type;
        let binary_op = get_infix_parser(token_type).unwrap();
        let rhs_precedence = match binary_op.associativity {
            Associativity::Left => binary_op.precedence.next_highest(),
            Associativity::Right => binary_op.precedence,
        };
        self.parse_precedence(rhs_precedence);
        match token_type {
            TokenType::Minus => self.emit_byte(OpCode::SUBTRACT as u8),
            TokenType::Plus => self.emit_byte(OpCode::ADD as u8),
            TokenType::Star => self.emit_byte(OpCode::MULIPLY as u8),
            TokenType::Slash => self.emit_byte(OpCode::DIVIDE as u8),
            TokenType::BangEqual => self.emit_bytes(OpCode::EQUAL as u8, OpCode::NOT as u8),
            TokenType::EqualEqual => self.emit_byte(OpCode::EQUAL as u8),
            TokenType::Greater => self.emit_byte(OpCode::GREATER as u8),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::LESS as u8, OpCode::NOT as u8),
            TokenType::Less => self.emit_byte(OpCode::LESS as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::GREATER as u8, OpCode::NOT as u8),
            _ => panic!("invalid binary token"),
        }
    }

    fn literal(&mut self) {
        match self.previous.token_type {
            TokenType::True => self.emit_byte(OpCode::TRUE as u8),
            TokenType::False => self.emit_byte(OpCode::FALSE as u8),
            TokenType::Nil => self.emit_byte(OpCode::NIL as u8),
            t => panic!("invalid literal type {:?}", t),
        }
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
            match get_infix_parser(self.current.token_type) {
                Some(binary_op) if binary_op.precedence >= precedence => {
                    self.advance();
                    (binary_op.parser)(self);
                }
                _ => break,
            }
        }
    }
}
