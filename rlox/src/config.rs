use std::{env, process::exit};

pub struct Config {
    pub file: Option<String>,
    pub debug_mode: bool,
}

impl Config {
    pub fn from_args() -> Config {
        let mut args = env::args();
        args.next(); // Skip over program name.
        let mut config = Config {
            file: None,
            debug_mode: false,
        };
        for arg in args {
            match arg.as_str() {
                "--debug" | "-d" => config.debug_mode = true,
                _ => {
                    if config.file.is_none() {
                        config.file = Some(arg);
                    } else {
                        usage();
                    }
                }
            }
        }
        config
    }
}

fn usage() -> ! {
    eprintln!("Usage: rlox [script] [-d|--debug]");
    exit(1);
}
