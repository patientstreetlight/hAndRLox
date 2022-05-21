use crate::scanner::{Scanner, TokenType};

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);
    let mut line = 0;
    let mut first_token = true;
    loop {
        let token = scanner.scan_token();
        if first_token || token.line != line {
            print!("{:4}", token.lexeme);
            line = token.line;
            first_token = false;
        } else {
            print!("   | ");
        }
        println!("{:?} {}", token.token_type, token.lexeme);
        if token.token_type == TokenType::EOF {
            break;
        }
    }
}