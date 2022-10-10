pub mod chunk;
pub mod compiler;
pub mod native;
pub mod scanner;
pub mod value;
pub mod vm;

use crate::chunk::Chunk;
use crate::value::Value;
use crate::vm::InterpretResult;
use crate::vm::VM;

use std::env;
use std::fs;
use std::io::{self, BufRead, Write};

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
        VM::interpret(&line);
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
    let result = VM::interpret(&file);
    match result {
        InterpretResult::CompileError => std::process::exit(65),
        InterpretResult::RuntimeError => std::process::exit(70),
        InterpretResult::OK => std::process::exit(0),
    }
}
