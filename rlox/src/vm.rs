use crate::chunk::OpCode;
use crate::compiler::compile;
use crate::native::ALL_NATIVE_FNS;
use crate::value::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

pub struct VM {
    stack: Vec<Value>,
    top_frame: Frame,
    frames: Vec<Frame>,
    globals: Vec<Option<Value>>,
    open_upvalues: HashMap<usize, UpValueRef>,
    debug_enabled: bool,
    allocator: GcAllocator,
}

struct Frame {
    closure: Gc<Closure>,
    base_pointer: usize,
    ip: usize,
}

pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

// XXX should print stack trace on error
impl VM {
    pub fn interpret(source: &str, debug_mode: bool) -> InterpretResult {
        let mut allocator = GcAllocator::new();
        let (global_ids, globals) = VM::define_natives(&mut allocator);
        let f = match compile(source, global_ids, &mut allocator) {
            None => return InterpretResult::CompileError,
            Some(f) => f,
        };
        let f = allocator.alloc_constant(f);
        let closure = Closure {
            function: f,
            upvalues: vec![],
        };
        let closure = allocator.alloc_constant(closure);
        let first_frame = Frame {
            closure,
            base_pointer: 0,
            ip: 0,
        };
        let mut vm = VM {
            stack: vec![Value::Closure(closure)],
            top_frame: first_frame,
            frames: Vec::new(),
            globals,
            open_upvalues: HashMap::new(),
            debug_enabled: debug_mode,
            allocator,
        };
        vm.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if self.debug_enabled {
                self.print_diagnostics();
            }
            let opcode = OpCode::from_u8(self.read_byte());
            match opcode {
                OpCode::CONSTANT => {
                    let constant_index = self.read_byte();
                    let constant =
                        self.top_frame.closure.function.chunk.constants[constant_index as usize];
                    self.push(constant);
                }
                OpCode::RETURN => {
                    let ret_val = self.pop();
                    while self.stack.len() > self.top_frame.base_pointer {
                        let top = self.stack.len() - 1;
                        if let Some(upvalue) = self.open_upvalues.remove(&top) {
                            *upvalue.borrow_mut() = UpValue::Closed(self.pop());
                        } else {
                            self.pop();
                        }
                    }
                    self.push(ret_val);
                    if let Some(frame) = self.frames.pop() {
                        self.top_frame = frame;
                    } else {
                        return InterpretResult::OK;
                    }
                }
                OpCode::NEGATE => {
                    let x = self.pop();
                    self.push(negate(x));
                }
                OpCode::ADD => {
                    let b = self.pop();
                    let a = self.pop();
                    let sum = add(a, b, &mut self.allocator);
                    self.push(sum);
                }
                OpCode::SUBTRACT => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(subtract(a, b));
                }
                OpCode::MULIPLY => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(multiply(a, b));
                }
                OpCode::DIVIDE => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(divide(a, b));
                }
                OpCode::NIL => self.push(Value::Nil),
                OpCode::TRUE => self.push(Value::Bool(true)),
                OpCode::FALSE => self.push(Value::Bool(false)),
                OpCode::NOT => {
                    let a = self.pop();
                    self.push(is_falsey(a));
                }
                OpCode::EQUAL => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.lox_eq(&b));
                }
                OpCode::GREATER => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(greater(a, b));
                }
                OpCode::LESS => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(less(a, b));
                }
                OpCode::PRINT => {
                    let a = self.pop();
                    println!("{}", a);
                }
                OpCode::POP => {
                    self.pop();
                }
                OpCode::DEF_GLOBAL => {
                    let global = self.read_byte() as usize;
                    for _ in self.globals.len()..=global {
                        self.globals.push(None);
                    }
                    let val = self.pop();
                    self.globals[global] = Some(val);
                }
                OpCode::GET_GLOBAL => {
                    let global = self.read_byte() as usize;
                    match self.globals.get(global) {
                        Some(Some(val)) => {
                            let val = *val;
                            self.push(val);
                        }
                        // XXX Need some kind of id -> name mapping for debugging purposes
                        _ => panic!("Undefined global variable"),
                    }
                }
                OpCode::SET_GLOBAL => {
                    let global = self.read_byte() as usize;
                    if global < self.globals.len() {
                        self.globals[global] = Some(self.peek());
                    } else {
                        panic!("Cannot assign to undeclared global variable");
                    }
                }
                OpCode::GET_LOCAL => {
                    let slot = self.read_byte() as usize;
                    self.push(self.stack[self.top_frame.base_pointer + slot]);
                }
                OpCode::SET_LOCAL => {
                    let slot = self.read_byte() as usize;
                    self.stack[self.top_frame.base_pointer + slot] = self.peek();
                }
                OpCode::JUMP_IF_FALSE => {
                    let offset = self.read_short() as usize;
                    if Self::is_falsey_bool(self.peek()) {
                        self.top_frame.ip += offset;
                    }
                }
                OpCode::JUMP => {
                    let offset = self.read_short() as usize;
                    self.top_frame.ip += offset;
                }
                OpCode::LOOP => {
                    let offset = self.read_short() as usize;
                    self.top_frame.ip -= offset;
                }
                OpCode::CALL => {
                    let num_args = self.read_byte() as usize;
                    let base_pointer = self.stack.len() - 1 - num_args;
                    let f = self.stack[base_pointer];
                    match f {
                        Value::Closure(closure) => {
                            if closure.function.arity != num_args as u8 {
                                panic!(
                                    "Expected {} args but got {}",
                                    closure.function.arity, num_args
                                );
                            }
                            let new_frame = Frame {
                                closure,
                                base_pointer,
                                ip: 0,
                            };
                            let old_frame = std::mem::replace(&mut self.top_frame, new_frame);
                            self.frames.push(old_frame);
                        }
                        Value::NativeFn(native) => {
                            let args = &mut self.stack[base_pointer + 1..];
                            let ret_val = (native.function)(args);
                            for _ in 0..num_args + 1 {
                                self.stack.pop();
                            }
                            self.push(ret_val);
                        }
                        Value::Class(class) => {
                            if num_args > 0 {
                                panic!("Not ready for args to constructors yet");
                            }
                            let instance = Instance::new(class);
                            let instance = self.allocator.alloc_constant(instance);
                            let instance = Value::Instance(instance);
                            self.stack[base_pointer] = instance;
                        }
                        _ => panic!("{f} is not a function"),
                    }
                }
                OpCode::CLOSURE => {
                    let f_idx = self.read_byte() as usize;
                    let f = self.top_frame.closure.function.chunk.constants[f_idx];
                    let f = match f {
                        Value::Function(f) => f,
                        _ => panic!("Can only wrap functions in closures"),
                    };
                    let upvalue_count = f.upvalue_count as usize;
                    let mut upvalues: Vec<UpValueRef> = Vec::with_capacity(upvalue_count);
                    let base_pointer = self.top_frame.base_pointer;
                    for _ in 0..upvalue_count {
                        let is_local = self.read_byte();
                        let is_local = is_local == 1;
                        let index = self.read_byte() as usize;
                        let upvalue = if is_local {
                            let upvalue_index = base_pointer + index;
                            match self.open_upvalues.get(&upvalue_index) {
                                Some(upvalue) => upvalue.clone(),
                                None => {
                                    let new_upvalue =
                                        Rc::new(RefCell::new(UpValue::Open(upvalue_index)));
                                    self.open_upvalues
                                        .insert(upvalue_index, new_upvalue.clone());
                                    new_upvalue
                                }
                            }
                        } else {
                            self.top_frame.closure.upvalues[index].clone()
                        };
                        upvalues.push(upvalue);
                    }
                    let closure = Closure {
                        function: f,
                        upvalues,
                    };
                    let closure = self.allocator.alloc_constant(closure);
                    self.push(Value::Closure(closure));
                }
                OpCode::CLOSE_UPVALUE => {
                    let upvalue_index = self.stack.len() - 1;
                    match self.open_upvalues.remove(&upvalue_index) {
                        None => panic!("Open upvalue not found in vm.open_upvalues"),
                        Some(upvalue) => *upvalue.borrow_mut() = UpValue::Closed(self.pop()),
                    }
                }
                OpCode::GET_UPVALUE => {
                    let upvalue_index = self.read_byte() as usize;
                    let upvalue_ref = self.top_frame.closure.upvalues[upvalue_index].clone();
                    match &*upvalue_ref.borrow_mut() {
                        UpValue::Open(location) => {
                            let val = self.stack[*location];
                            self.push(val);
                        }
                        UpValue::Closed(val) => {
                            let val = *val;
                            self.push(val);
                        }
                    };
                }
                OpCode::SET_UPVALUE => {
                    let upvalue_index = self.read_byte() as usize;
                    let upvalue_ref = self.top_frame.closure.upvalues[upvalue_index].clone();
                    let val = self.peek();
                    match *upvalue_ref.borrow_mut() {
                        UpValue::Open(location) => {
                            self.stack[location] = val;
                        }
                        UpValue::Closed(ref mut old_val) => {
                            *old_val = val;
                        }
                    };
                }
                OpCode::CLASS => {
                    let name = self.read_string_constant();
                    let class = Class { name };
                    let class = self.allocator.alloc_constant(class);
                    let class = Value::Class(class);
                    self.push(class);
                }
                OpCode::SET_PROPERTY => {
                    let prop_name = self.read_string_constant();
                    let val = self.pop();
                    let obj = self.pop();
                    if let Value::Instance(mut instance) = obj {
                        instance.set_property(&prop_name, val);
                        self.push(val);
                    } else {
                        panic!("Only instances have fields");
                    }
                }
                OpCode::GET_PROPERTY => {
                    let prop_name = self.read_string_constant();
                    let obj = self.pop();
                    if let Value::Instance(instance) = obj {
                        let val = instance.get_property(&prop_name);
                        self.push(val);
                    } else {
                        panic!("Only instances have fields");
                    }
                }
            }
        }
    }

    // Corresponds to READ_STRING() macro in CI.
    fn read_string_constant(&mut self) -> Gc<String> {
        let index = self.read_byte() as usize;
        let v = self.top_frame.closure.function.chunk.constants[index];
        if let Value::Str(s) = v {
            s
        } else {
            panic!("Expected string constant, got {:?}", v)
        }
    }

    fn read_byte(&mut self) -> u8 {
        let b = self.top_frame.closure.function.chunk.code[self.top_frame.ip];
        self.top_frame.ip += 1;
        b
    }

    fn is_falsey_bool(v: Value) -> bool {
        matches!(v, Value::Nil | Value::Bool(false))
    }

    fn read_short(&mut self) -> u16 {
        let hi_byte = self.top_frame.closure.function.chunk.code[self.top_frame.ip] as u16;
        let lo_byte = self.top_frame.closure.function.chunk.code[self.top_frame.ip + 1] as u16;
        self.top_frame.ip += 2;
        hi_byte << 8 | lo_byte
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("popped an empty stack")
    }

    fn peek(&self) -> Value {
        self.stack[self.stack.len() - 1]
    }

    fn define_natives(allocator: &mut GcAllocator) -> (HashMap<String, u8>, Vec<Option<Value>>) {
        let mut natives = HashMap::new();
        let mut globals: Vec<Option<Value>> = vec![];
        for (name, f) in ALL_NATIVE_FNS {
            natives.insert(name.to_string(), globals.len() as u8);
            let native_fn = NativeFn {
                name: name.to_string(),
                function: f,
            };
            let native_fn = allocator.alloc_constant(native_fn);
            let native_fn = Value::NativeFn(native_fn);
            globals.push(Some(native_fn));
        }
        (natives, globals)
    }

    fn print_diagnostics(&self) {
        println!("\n============");
        println!("stack");
        println!("-----");
        let base_pointer = self.top_frame.base_pointer;
        for (i, v) in self.stack.iter().enumerate() {
            let bp_marker = if i == base_pointer { "*" } else { " " };
            println!("{} {:4} {:?}", bp_marker, i, v);
        }
        if !self.globals.is_empty() {
            println!("globals");
            println!("-------");
            for (i, v) in self.globals.iter().enumerate() {
                if let Some(v) = v {
                    println!("{:4} {:?}", i, v);
                }
            }
        }
        if !self.open_upvalues.is_empty() {
            println!("open upvalues");
            println!("-------------");
            for (k, v) in self.open_upvalues.iter() {
                let v = v.borrow_mut();
                println!("{:4} {:?}", k, v);
            }
        }
        println!("curr fun: {:?}", self.top_frame.closure);
        println!("constants");
        println!("---------");
        for (i, v) in self
            .top_frame
            .closure
            .function
            .chunk
            .constants
            .iter()
            .enumerate()
        {
            println!("{:4} {:?}", i, v);
        }
        println!("code");
        println!("----");
        let mut chunk_iter = self
            .top_frame
            .closure
            .function
            .chunk
            .code
            .iter()
            .enumerate();
        let ip = self.top_frame.ip;
        while let Some((i, &inst)) = chunk_iter.next() {
            let mut print_byte = || {
                let (_, &c) = chunk_iter.next().unwrap();
                println!("  {c}");
            };
            let opcode = OpCode::from_u8(inst);
            let marker = if ip == i { "* " } else { "  " };
            print!("{marker}");
            match opcode {
                OpCode::CONSTANT => {
                    println!("CONSTANT");
                    print_byte();
                }
                OpCode::NEGATE => println!("NEGATE"),
                OpCode::ADD => println!("ADD"),
                OpCode::SUBTRACT => println!("SUBTRACT"),
                OpCode::MULIPLY => println!("MULTIPLY"),
                OpCode::DIVIDE => println!("DIVIDE"),
                OpCode::RETURN => println!("RETURN"),
                OpCode::NIL => println!("NIL"),
                OpCode::TRUE => println!("TRUE"),
                OpCode::FALSE => println!("FALSE"),
                OpCode::NOT => println!("NOT"),
                OpCode::EQUAL => println!("EQUAL"),
                OpCode::GREATER => println!("GREATER"),
                OpCode::LESS => println!("LESS"),
                OpCode::PRINT => println!("PRINT"),
                OpCode::POP => println!("POP"),
                OpCode::DEF_GLOBAL => {
                    println!("DEF_GLOBAL");
                    print_byte();
                }
                OpCode::GET_GLOBAL => {
                    println!("GET_GLOBAL");
                    print_byte();
                }
                OpCode::SET_GLOBAL => {
                    println!("SET_GLOBAL");
                    print_byte();
                }
                OpCode::GET_LOCAL => {
                    println!("GET_LOCAL");
                    print_byte();
                }
                OpCode::SET_LOCAL => {
                    println!("SET_LOCAL");
                    print_byte();
                }
                OpCode::JUMP_IF_FALSE => {
                    println!("JUMP_IF_FALSE");
                    let (_, &a) = chunk_iter.next().unwrap();
                    let (_, &b) = chunk_iter.next().unwrap();
                    let short: u16 = (a as u16) << 8 | b as u16;
                    println!("  {short}");
                }
                OpCode::JUMP => {
                    println!("JUMP");
                    let (_, &a) = chunk_iter.next().unwrap();
                    let (_, &b) = chunk_iter.next().unwrap();
                    let short: u16 = (a as u16) << 8 | b as u16;
                    println!("  {short}");
                }
                OpCode::LOOP => {
                    println!("LOOP");
                    let (_, &a) = chunk_iter.next().unwrap();
                    let (_, &b) = chunk_iter.next().unwrap();
                    let short: u16 = (a as u16) << 8 | b as u16;
                    println!("  {short}");
                }
                OpCode::CALL => {
                    println!("CALL");
                    print_byte();
                }
                OpCode::CLOSURE => {
                    println!("CLOSURE");
                    let (_, &f_idx) = chunk_iter.next().unwrap();
                    let f = &self.top_frame.closure.function.chunk.constants[f_idx as usize];
                    let f = match f {
                        Value::Function(f) => f,
                        _ => panic!("Can only wrap functions in closures"),
                    };
                    println!("  {f_idx} ({:?})", f);
                    let upvalue_count = f.upvalue_count;
                    for _ in 0..upvalue_count {
                        let (_, &is_local) = chunk_iter.next().unwrap();
                        let (_, &index) = chunk_iter.next().unwrap();
                        println!("  {is_local}");
                        println!("  {index}");
                    }
                }
                OpCode::GET_UPVALUE => {
                    println!("GET_UPVALUE");
                    print_byte();
                }
                OpCode::SET_UPVALUE => {
                    println!("SET_UPVALUE");
                    print_byte();
                }
                OpCode::CLOSE_UPVALUE => println!("CLOSE_UPVALUE"),
                OpCode::CLASS => {
                    println!("CLASS");
                    let (_, &name_index) = chunk_iter.next().unwrap();
                    let name = self.top_frame.closure.function.chunk.constants[name_index as usize];
                    println!("  {name_index} ({name})");
                }
                OpCode::SET_PROPERTY => {
                    println!("SET_PROPERTY");
                    let (_, &name_index) = chunk_iter.next().unwrap();
                    let name = self.top_frame.closure.function.chunk.constants[name_index as usize];
                    println!("  {name_index} ({name})");
                }
                OpCode::GET_PROPERTY => {
                    println!("GET_PROPERTY");
                    let (_, &name_index) = chunk_iter.next().unwrap();
                    let name = self.top_frame.closure.function.chunk.constants[name_index as usize];
                    println!("  {name_index} ({name})");
                }
            }
        }
    }
}
