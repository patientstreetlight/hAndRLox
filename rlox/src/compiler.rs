use crate::chunk::{Chunk, OpCode};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Function;
use crate::value::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub fn compile(source: &str, globals: HashMap<String, u8>) -> Option<Function> {
    let mut parser = Parser::new(source, globals);
    while !parser.try_match(TokenType::EOF) {
        parser.declaration();
    }
    parser.end_compiler()
}

struct Local<'a> {
    name: &'a str,
    depth: usize,
    initialized: bool,
}

#[derive(std::cmp::PartialEq)]
enum FunctionType {
    Function,
    Script,
}

struct Compiler<'a> {
    curr_fn_compiler: FnCompiler<'a>,
    fn_compilers: Vec<FnCompiler<'a>>,
    globals: HashMap<String, u8>,
}

struct FnCompiler<'a> {
    function: Function,
    function_type: FunctionType,
    locals: Vec<Local<'a>>,
    scope_depth: usize,
}

impl<'a> FnCompiler<'a> {
    fn new(function_type: FunctionType) -> FnCompiler<'a> {
        FnCompiler {
            function: Function::new(),
            function_type,
            // The bottom of the frame will always be the function object itself,
            // so it's unavailable for local variable use.  It won't interfere with any
            // actual variables since real variables cannot have an empty name.
            locals: vec![Local {
                name: "",
                depth: 0,
                initialized: true,
            }],
            scope_depth: 0,
        }
    }
}

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
    scanner: Scanner<'a>,
    had_error: bool,
    panic_mode: bool,
    compiler: Compiler<'a>,
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
    precedence: Precedence,
    parser: fn(&mut Parser<'a>),
}

fn get_infix_parser<'a>(token_type: TokenType) -> Option<BinaryOp<'a>> {
    match token_type {
        TokenType::Minus => Some(BinaryOp {
            precedence: Precedence::Term,
            parser: Parser::binary,
        }),
        TokenType::Plus => Some(BinaryOp {
            precedence: Precedence::Term,
            parser: Parser::binary,
        }),
        TokenType::Star => Some(BinaryOp {
            precedence: Precedence::Factor,
            parser: Parser::binary,
        }),
        TokenType::Slash => Some(BinaryOp {
            precedence: Precedence::Factor,
            parser: Parser::binary,
        }),
        TokenType::BangEqual => Some(BinaryOp {
            precedence: Precedence::Equality,
            parser: Parser::binary,
        }),
        TokenType::EqualEqual => Some(BinaryOp {
            precedence: Precedence::Equality,
            parser: Parser::binary,
        }),
        TokenType::Greater => Some(BinaryOp {
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        TokenType::GreaterEqual => Some(BinaryOp {
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        TokenType::Less => Some(BinaryOp {
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        TokenType::LessEqual => Some(BinaryOp {
            precedence: Precedence::Comparison,
            parser: Parser::binary,
        }),
        TokenType::And => Some(BinaryOp {
            precedence: Precedence::And,
            parser: Parser::and,
        }),
        TokenType::Or => Some(BinaryOp {
            precedence: Precedence::Or,
            parser: Parser::or,
        }),
        TokenType::LeftParen => Some(BinaryOp {
            precedence: Precedence::Call,
            parser: Parser::call,
        }),
        _ => None,
    }
}

fn get_prefix_parser<'a>(token_type: TokenType) -> Option<fn(&mut Parser<'a>, bool)> {
    match token_type {
        TokenType::LeftParen => Some(Parser::grouping),
        TokenType::Minus => Some(Parser::unary),
        TokenType::Number => Some(Parser::number),
        TokenType::True => Some(Parser::literal),
        TokenType::False => Some(Parser::literal),
        TokenType::Nil => Some(Parser::literal),
        TokenType::Bang => Some(Parser::unary),
        TokenType::String => Some(Parser::string),
        TokenType::Identifier => Some(Parser::variable),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, globals: HashMap<String, u8>) -> Parser<'a> {
        let mut scanner = Scanner::new(source);
        let first_token = scanner.scan_token();
        let fn_compiler = FnCompiler::new(FunctionType::Script);
        let compiler = Compiler {
            curr_fn_compiler: fn_compiler,
            fn_compilers: Vec::new(),
            globals,
        };
        Parser {
            previous: first_token,
            current: first_token,
            scanner,
            had_error: false,
            panic_mode: false,
            compiler,
        }
    }

    fn is_top_level(&self) -> bool {
        self.compiler.curr_fn_compiler.function_type == FunctionType::Script
            && self.compiler.curr_fn_compiler.scope_depth == 0
    }

    // This roughly corresponds to initCompiler() in CI.
    // popped by end_fn_compiler()
    fn push_new_fn_compiler(&mut self) {
        let name = self.previous.lexeme.to_string();
        let mut function = Function::new();
        function.name = Some(name);
        let fn_compiler = FnCompiler::new(FunctionType::Function);
        let prev_fn_compiler = std::mem::replace(&mut self.compiler.curr_fn_compiler, fn_compiler);
        self.compiler.fn_compilers.push(prev_fn_compiler);
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.compiler.curr_fn_compiler.function.chunk
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

    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::CALL as u8, arg_count);
    }

    fn argument_list(&mut self) -> u8 {
        let mut num_args = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                if num_args == 255 {
                    self.error("Can't have more than 255 arguments");
                }
                num_args += 1;
                self.expression();
                if !self.try_match(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after function arguments");
        num_args
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) {
        if self.try_match(TokenType::Fun) {
            self.fn_declaration();
        } else if self.try_match(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }

    fn fn_declaration(&mut self) {
        let global = self.parse_variable();
        self.mark_initialized();
        self.function();
        self.define_variable(global);
    }

    // parser is sitting on the opening '(' after a function name.
    // Should emit code which leaves a new function on top of the stack.
    fn function(&mut self) {
        self.push_new_fn_compiler();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                if self.compiler.curr_fn_compiler.function.arity == 255 {
                    self.error_at_current("Cannot have more than 255 parameters");
                }
                self.compiler.curr_fn_compiler.function.arity += 1;
                let constant = self.parse_variable();
                self.define_variable(constant);
                if !self.try_match(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "Expect ')' after function parameter list",
        );
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();
        let function = self.end_fn_compiler();
        let function = Value::Function(Rc::new(function));
        self.emit_constant(function);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable();
        if self.try_match(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::NIL as u8);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        );
        self.define_variable(global);
    }

    fn define_variable(&mut self, idx: u8) {
        if !self.is_top_level() {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DEF_GLOBAL as u8, idx);
    }

    fn mark_initialized(&mut self) {
        if self.is_top_level() {
            return;
        }
        let num_locals = self.compiler.curr_fn_compiler.locals.len();
        self.compiler.curr_fn_compiler.locals[num_locals - 1].initialized = true;
    }

    // If at top level, resolves the global variable name and returns its handle.
    // Otherwise, adds local variable to locals (i.e. declares it) and returns 0.
    fn parse_variable(&mut self) -> u8 {
        self.consume(TokenType::Identifier, "Expected identifier");
        self.declare_variable();
        if !self.is_top_level() {
            // XXX Is this right?
            return 0;
        }
        let var_name = self.previous.lexeme;
        self.resolve_global(var_name)
    }

    fn declare_variable(&mut self) {
        if self.is_top_level() {
            return;
        }
        let name = self.previous.lexeme;
        let conflict = self
            .compiler
            .curr_fn_compiler
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth == self.compiler.curr_fn_compiler.scope_depth)
            .any(|l| l.name == name);
        if conflict {
            self.error("Already a variable with this name in this scope.");
        }
        self.add_local(name);
    }

    fn add_local(&mut self, name: &'a str) {
        let local = Local {
            name,
            depth: self.compiler.curr_fn_compiler.scope_depth,
            initialized: false,
        };
        self.compiler.curr_fn_compiler.locals.push(local);
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current.token_type != TokenType::EOF {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }
            match self.current.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => self.advance(),
            }
        }
    }

    fn statement(&mut self) {
        if self.try_match(TokenType::Print) {
            self.print_statement();
        } else if self.try_match(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.try_match(TokenType::If) {
            self.if_statement();
        } else if self.try_match(TokenType::While) {
            self.while_statement();
        } else if self.try_match(TokenType::For) {
            self.for_statement();
        } else if self.try_match(TokenType::Return) {
            self.return_statement();
        } else {
            self.expression_statement();
        }
    }

    fn return_statement(&mut self) {
        if self.compiler.curr_fn_compiler.function_type == FunctionType::Script {
            self.error("Cannot return from top-level");
        }
        if self.try_match(TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value");
            self.emit_byte(OpCode::RETURN as u8);
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression");
        self.emit_byte(OpCode::POP as u8);
    }

    fn for_statement(&mut self) {
        // XXX Add support for break/continue.  Also to while loops.
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        // initializer
        if self.try_match(TokenType::Semicolon) {
        } else if self.try_match(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }
        // test
        let mut loop_start = self.current_loc();
        let mut test_to_loop_end: Option<usize> = None;
        if !self.try_match(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ; after test");
            test_to_loop_end = Some(self.emit_jump(OpCode::JUMP_IF_FALSE));
            self.emit_byte(OpCode::POP as u8);
        }
        if !self.try_match(TokenType::RightParen) {
            let loop_body = self.emit_jump(OpCode::JUMP);
            let increment_start = self.current_loc();
            self.expression();
            self.emit_byte(OpCode::POP as u8);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses");
            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(loop_body);
        }
        // loop body
        self.statement();
        self.emit_loop(loop_start);
        // end
        if let Some(test_to_loop_end) = test_to_loop_end {
            self.patch_jump(test_to_loop_end);
            self.emit_byte(OpCode::POP as u8);
        }
        self.end_scope();
    }

    fn current_loc(&mut self) -> usize {
        self.current_chunk().code.len()
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_loc();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after while condition");
        let jump_to_end = self.emit_jump(OpCode::JUMP_IF_FALSE);
        self.emit_byte(OpCode::POP as u8);
        self.statement();
        self.emit_loop(loop_start);
        self.patch_jump(jump_to_end);
        self.emit_byte(OpCode::POP as u8);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::LOOP as u8);
        let jump = self.current_loc() + 2 - loop_start;
        let jump = jump as u16;
        let hi = (jump >> 8) & 0xff;
        let lo = jump & 0xff;
        self.emit_byte(hi as u8);
        self.emit_byte(lo as u8);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after if");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after if confition");
        let jump_to_else = self.emit_jump(OpCode::JUMP_IF_FALSE);
        // start of then branch
        self.emit_byte(OpCode::POP as u8);
        self.statement();
        let jump_to_end = self.emit_jump(OpCode::JUMP);
        // start of else branch
        self.patch_jump(jump_to_else);
        self.emit_byte(OpCode::POP as u8);
        if self.try_match(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(jump_to_end);
    }

    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit_byte(op as u8);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.current_loc() - 2
    }

    fn patch_jump(&mut self, patch: usize) {
        let patch = patch as usize;
        let jump_distance = self.current_loc() - patch - 2;
        let hi = ((jump_distance >> 8) & 0xff) as u8;
        let lo = (jump_distance & 0xff) as u8;
        self.current_chunk().code[patch] = hi;
        self.current_chunk().code[patch + 1] = lo;
    }

    fn block(&mut self) {
        while !self.check(TokenType::EOF) && !self.check(TokenType::RightBrace) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn begin_scope(&mut self) {
        self.compiler.curr_fn_compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.curr_fn_compiler.scope_depth -= 1;
        loop {
            match self.compiler.curr_fn_compiler.locals.last() {
                Some(l) if l.depth > self.compiler.curr_fn_compiler.scope_depth => {
                    self.compiler.curr_fn_compiler.locals.pop();
                    self.emit_byte(OpCode::POP as u8);
                }
                _ => break,
            }
        }
    }

    fn try_match(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        self.current.token_type == token_type
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after expression for print",
        );
        self.emit_byte(OpCode::PRINT as u8);
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
        let prev_line = self.previous.line as usize;
        self.current_chunk().write(b, prev_line);
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8) {
        self.emit_byte(b1);
        self.emit_byte(b2);
    }

    fn end_compiler(mut self) -> Option<Function> {
        self.emit_return();
        if self.had_error {
            None
        } else {
            Some(self.compiler.curr_fn_compiler.function)
        }
    }

    fn end_fn_compiler(&mut self) -> Function {
        self.emit_return();
        if let Some(fn_compiler) = self.compiler.fn_compilers.pop() {
            let prev_fn_compiler =
                std::mem::replace(&mut self.compiler.curr_fn_compiler, fn_compiler);
            prev_fn_compiler.function
        } else {
            panic!("Cannot call end_fn_compiler() on the top-level script");
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::NIL as u8);
        self.emit_byte(OpCode::RETURN as u8);
    }

    fn number(&mut self, _can_assign: bool) {
        let n: f64 = self.previous.lexeme.parse().unwrap();
        self.emit_constant(Value::Num(n));
    }

    fn string(&mut self, _can_assign: bool) {
        // The lexeme contains the initial and trailing " characters which need
        // to be stripped.
        let lexeme = self.previous.lexeme;
        let stripped_lexeme = &lexeme[1..lexeme.len() - 1];
        let s = String::from(stripped_lexeme);
        let rc_s = Rc::new(s);
        let lox_s = Value::Str(rc_s);
        self.emit_constant(lox_s);
    }

    fn emit_constant(&mut self, val: Value) {
        let prev_line = self.previous.line as usize;
        self.current_chunk().write_constant(val, prev_line);
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.token_type;
        self.parse_precedence(Precedence::Unary);
        let op = match operator_type {
            TokenType::Minus => OpCode::NEGATE,
            TokenType::Bang => OpCode::NOT,
            _ => panic!("bad unary operator type"),
        };
        self.emit_byte(op as u8);
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign);
    }

    fn named_variable(&mut self, name_token: Token, can_assign: bool) {
        let name = name_token.lexeme;
        let (get_op, set_op, arg) = match self.resolve_local(name) {
            Some(local_index) => (OpCode::GET_LOCAL, OpCode::SET_LOCAL, local_index),
            None => {
                let global = self.resolve_global(name_token.lexeme);
                (OpCode::GET_GLOBAL, OpCode::SET_GLOBAL, global)
            }
        };
        if can_assign && self.try_match(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op as u8, arg);
        } else {
            self.emit_bytes(get_op as u8, arg);
        }
    }

    fn resolve_local(&mut self, name: &str) -> Option<u8> {
        for (i, l) in self
            .compiler
            .curr_fn_compiler
            .locals
            .iter()
            .enumerate()
            .rev()
        {
            if l.name == name {
                if !l.initialized {
                    self.error("Can't read local variable in its own initializer.");
                }
                return Some(i as u8);
            }
        }
        None
    }

    fn resolve_global(&mut self, name: &str) -> u8 {
        match self.compiler.globals.get(name) {
            Some(&id) => id,
            None => {
                let id = self.compiler.globals.len();
                if id > 255 {
                    panic!("Too many globals");
                }
                let id = id as u8;
                self.compiler.globals.insert(String::from(name), id);
                id
            }
        }
    }

    fn binary(&mut self) {
        let token_type = self.previous.token_type;
        let binary_op = get_infix_parser(token_type).unwrap();
        let rhs_precedence = binary_op.precedence.next_highest();
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

    fn and(&mut self) {
        let end_jump = self.emit_jump(OpCode::JUMP_IF_FALSE);
        self.emit_byte(OpCode::POP as u8);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self) {
        let continue_jump = self.emit_jump(OpCode::JUMP_IF_FALSE);
        // lhs was true
        let end_jump = self.emit_jump(OpCode::JUMP);
        // lhs was false
        self.patch_jump(continue_jump);
        self.emit_byte(OpCode::POP as u8);
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous.token_type {
            TokenType::True => self.emit_byte(OpCode::TRUE as u8),
            TokenType::False => self.emit_byte(OpCode::FALSE as u8),
            TokenType::Nil => self.emit_byte(OpCode::NIL as u8),
            t => panic!("invalid literal type {:?}", t),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        let can_assign = precedence <= Precedence::Assignment;
        self.advance();
        match get_prefix_parser(self.previous.token_type) {
            Some(prefix_rule) => prefix_rule(self, can_assign),
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
        if can_assign && self.try_match(TokenType::Equal) {
            self.error("Invalid assignment target");
        }
    }
}
