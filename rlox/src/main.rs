pub mod chunk;
pub mod value;
pub mod vm;
pub mod compiler;
pub mod scanner;

use crate::chunk::Chunk;
use crate::chunk::Instruction;
use crate::value::Value;
use crate::vm::VM;
use crate::vm::InterpretResult;

use std::env;
use std::io::{self, BufRead, Write};
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => panic!("Usage: rlox [path]"),
    }
}

fn repl() {
    let mut line = String::new();
    loop {
        line.clear();
        print!("> ");
        io::stdout().flush();
        let read_result = io::stdin().lock().read_line(&mut line);
        if let Err(_) = read_result {
            println!("");
            break;
        }
        VM::interpret2(&line);
    }
}

fn run_file(script_file: &str) {
    let file = fs::read_to_string(script_file);
    let file = match file {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Coult not open file\"{script_file}\": {}", e);
            std::process::exit(74);
        }
    };
    let result = VM::interpret2(&file);
    match result {
        InterpretResult::CompileError => std::process::exit(65),
        InterpretResult::RuntimeError => std::process::exit(70),
        InterpretResult::OK => std::process::exit(0),
    }
}