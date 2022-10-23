// XXX Consider making Scanner an iterator of Tokens
pub struct Scanner<'a> {
    start: &'a str,
    line: u32,
    current_token_len: usize,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub line: u32,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    // Single character tokens:
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens:
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals:
    Identifier,
    String,
    Number,
    // Keywords:
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    EOF,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Scanner {
            start: source,
            line: 1,
            current_token_len: 0,
        }
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = &self.start[self.current_token_len..];
        self.current_token_len = 0;
        if self.is_at_end() {
            return self.mk_token(TokenType::EOF);
        }
        let c = self.advance();
        match c {
            _ if is_alpha(c) => self.identifier(),
            _ if is_digit(c) => self.number(),
            '(' => self.mk_token(TokenType::LeftParen),
            ')' => self.mk_token(TokenType::RightParen),
            '{' => self.mk_token(TokenType::LeftBrace),
            '}' => self.mk_token(TokenType::RightBrace),
            ';' => self.mk_token(TokenType::Semicolon),
            ',' => self.mk_token(TokenType::Comma),
            '.' => self.mk_token(TokenType::Dot),
            '-' => self.mk_token(TokenType::Minus),
            '+' => self.mk_token(TokenType::Plus),
            '/' => self.mk_token(TokenType::Slash),
            '*' => self.mk_token(TokenType::Star),
            '!' => self.try_match_equal(TokenType::BangEqual, TokenType::Bang),
            '=' => self.try_match_equal(TokenType::EqualEqual, TokenType::Equal),
            '<' => self.try_match_equal(TokenType::LessEqual, TokenType::Less),
            '>' => self.try_match_equal(TokenType::GreaterEqual, TokenType::Greater),
            '"' => self.string(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        loop {
            let c = self.peek();
            match c {
                Some(c) if is_alpha(c) || is_digit(c) => self.current_token_len += 1,
                _ => break,
            }
        }
        self.mk_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        let lexeme = &self.start[0..self.current_token_len];
        match lexeme {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        }
    }

    fn number(&mut self) -> Token<'a> {
        self.consume_digits();
        let a = self.peek();
        let b = self.peek_next();
        match (a, b) {
            (Some('.'), Some(c)) if is_digit(c) => {
                // Consumes the '.'
                self.current_token_len += 1;
                self.consume_digits();
            }
            _ => (),
        }
        self.mk_token(TokenType::Number)
    }

    fn consume_digits(&mut self) {
        loop {
            let c = self.peek();
            match c {
                Some(c) if is_digit(c) => self.current_token_len += 1,
                _ => break,
            }
        }
    }

    fn string(&mut self) -> Token<'a> {
        loop {
            match self.peek() {
                Some('"') => {
                    self.current_token_len += 1;
                    return self.mk_token(TokenType::String);
                }
                Some(_) => self.current_token_len += 1,
                None => return self.error_token("Unterminated string."),
            }
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                Some(' ') | Some('\r') | Some('\t') => self.current_token_len += 1,
                Some('\n') => {
                    self.line += 1;
                    self.current_token_len += 1;
                }
                Some('/') => match self.peek_next() {
                    Some('/') => {
                        while !self.is_at_end() && self.peek() != Some('\n') {
                            self.current_token_len += 1;
                        }
                    }
                    _ => return,
                },
                _ => return,
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.rest_chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        self.rest_chars().skip(1).next()
    }

    fn try_match_equal(&mut self, if_match: TokenType, if_not_match: TokenType) -> Token<'a> {
        let token_type = if self.try_match('=') {
            if_match
        } else {
            if_not_match
        };
        self.mk_token(token_type)
    }

    fn rest_chars(&self) -> std::str::Chars {
        self.start[self.current_token_len..].chars()
    }

    fn try_match(&mut self, desired: char) -> bool {
        match self.peek() {
            Some(c) if c == desired => {
                self.current_token_len += 1;
                true
            }
            _ => false,
        }
    }

    fn is_at_end(&self) -> bool {
        self.start.len() == self.current_token_len
    }

    fn mk_token(&mut self, token_type: TokenType) -> Token<'a> {
        Token {
            token_type,
            lexeme: &self.start[0..self.current_token_len],
            line: self.line,
        }
    }

    fn error_token(&mut self, msg: &'static str) -> Token<'static> {
        Token {
            token_type: TokenType::Error,
            lexeme: msg,
            line: self.line,
        }
    }

    fn advance(&mut self) -> char {
        let c = self
            .rest_chars()
            .next()
            .expect("advance() called when there are no more characters");
        self.current_token_len += 1;
        c
    }
}

fn is_alpha(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    }
}

fn is_digit(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn num_without_decimal_point() {
        let mut scanner = Scanner::new("1234 ");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "1234",
            line: 1,
            token_type: TokenType::Number,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn num_with_decimal_point() {
        let mut scanner = Scanner::new("1234.56 ");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "1234.56",
            line: 1,
            token_type: TokenType::Number,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn num_followed_by_dot() {
        let mut scanner = Scanner::new("1234.x");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "1234",
            line: 1,
            token_type: TokenType::Number,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn string() {
        let mut scanner = Scanner::new("\"foo\" ");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "\"foo\"",
            line: 1,
            token_type: TokenType::String,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn unterminated_string() {
        let mut scanner = Scanner::new("\"foo ");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "Unterminated string.",
            line: 1,
            token_type: TokenType::Error,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn whitespace() {
        let mut scanner = Scanner::new(" \t // a comment\n \t1234");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "1234",
            line: 2,
            token_type: TokenType::Number,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn eof() {
        let mut scanner = Scanner::new("");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "",
            line: 1,
            token_type: TokenType::EOF,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn with_equals() {
        let cases = [
            ("!", TokenType::Bang, TokenType::BangEqual),
            ("=", TokenType::Equal, TokenType::EqualEqual),
            (">", TokenType::Greater, TokenType::GreaterEqual),
            ("<", TokenType::Less, TokenType::LessEqual),
        ];
        for (prefix, tok_without_eq, tok_with_eq) in cases {
            let mut scanner = Scanner::new(prefix);
            let tok = scanner.scan_token();
            assert_eq!(tok.token_type, tok_without_eq);
            assert_eq!(tok.lexeme, prefix);

            let with_eq = format!("{prefix}=");
            let mut scanner = Scanner::new(&with_eq);
            let tok = scanner.scan_token();
            assert_eq!(tok.token_type, tok_with_eq);
            assert_eq!(tok.lexeme, &with_eq);
        }
    }

    #[test]
    fn identifier() {
        let mut scanner = Scanner::new("fork123_=");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "fork123_",
            line: 1,
            token_type: TokenType::Identifier,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn keyword() {
        let mut scanner = Scanner::new("for(");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "for",
            line: 1,
            token_type: TokenType::For,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn test_true() {
        let mut scanner = Scanner::new("true");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "true",
            line: 1,
            token_type: TokenType::True,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn test_false() {
        let mut scanner = Scanner::new("false");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "false",
            line: 1,
            token_type: TokenType::False,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn test_nil() {
        let mut scanner = Scanner::new("nil");
        let tok = scanner.scan_token();
        let expected = Token {
            lexeme: "nil",
            line: 1,
            token_type: TokenType::Nil,
        };
        assert_eq!(tok, expected);
    }

    #[test]
    fn sequence() {
        let mut scanner = Scanner::new("var x = 10;");
        let expected_tokens = [
            ("var", TokenType::Var),
            ("x", TokenType::Identifier),
            ("=", TokenType::Equal),
            ("10", TokenType::Number),
            (";", TokenType::Semicolon),
            ("", TokenType::EOF),
        ];
        for (lexeme, expected_type) in expected_tokens {
            let tok = scanner.scan_token();
            let expected = Token {
                lexeme,
                line: 1,
                token_type: expected_type,
            };
            assert_eq!(tok, expected);
        }
    }
}
