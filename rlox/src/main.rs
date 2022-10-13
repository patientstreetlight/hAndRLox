pub mod chunk;
pub mod compiler;
pub mod config;
pub mod native;
pub mod scanner;
pub mod value;
pub mod vm;

use crate::chunk::Chunk;
use crate::config::Config;
use crate::value::Value;
use crate::vm::InterpretResult;
use crate::vm::VM;

use std::fs;
use std::io::{self, BufRead, Write};

fn main() {
    let config = Config::from_args();
    if let Some(script_file) = config.file {
        run_file(&script_file, config.debug_mode);
    } else {
        repl();
    }
}

fn repl() {
    let mut line = String::new();
    loop {
        line.clear();
        print!("> ");
        io::stdout().flush().unwrap();
        let read_result = io::stdin().lock().read_line(&mut line);
        if read_result.is_err() {
            println!();
            break;
        }
        VM::interpret(&line, false);
    }
}

fn run_file(script_file: &str, debug_mode: bool) {
    let file = fs::read_to_string(script_file);
    let file = match file {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Coult not open file\"{script_file}\": {}", e);
            std::process::exit(74);
        }
    };
    let result = VM::interpret(&file, debug_mode);
    match result {
        InterpretResult::CompileError => std::process::exit(65),
        InterpretResult::RuntimeError => std::process::exit(70),
        InterpretResult::OK => std::process::exit(0),
    }
}
