use crate::Chunk;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
    Str(Rc<String>),
    // XXX Should probably be GC'd rather than RC'd
    Function(Rc<Function>),
    NativeFn(Rc<NativeFn>),
    Closure(Rc<Closure>),
}

// UpValue invariants:
// - a given local variable is captured by at most 1 UpValue.
// - all closures which capture the same local variable do so
//   through the same UpValue.

pub type UpValueRef = Rc<RefCell<UpValue>>;

#[derive(Debug)]
pub enum UpValue {
    Open(usize),
    Closed(Value),
}

pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<UpValueRef>,
}

impl Closure {
    pub fn new(function: Rc<Function>, upvalues: Vec<UpValueRef>) -> Rc<Closure> {
        Rc::new(Closure { function, upvalues })
    }
}

impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.function.fmt(f)
    }
}

pub struct NativeFn {
    pub name: String,
    pub function: fn(&mut [Value]) -> Value,
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}

pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Option<String>,
    pub upvalue_count: u8,
}

impl Function {
    pub fn new() -> Function {
        Function {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
            upvalue_count: 0,
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.name.as_ref() {
            None => write!(f, "<script>"),
            Some(name) => write!(f, "<fn {}>", name),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
            Self::Str(s) => write!(f, "{}", s.as_ref()),
            Self::Function(function) => match &function.name {
                None => write!(f, "<script>"),
                Some(name) => write!(f, "<fn {}>", name),
            },
            Self::NativeFn(function) => write!(f, "<native fn {}>", function.name),
            Self::Closure(_closure) => write!(f, "<closure>"),
        }
    }
}

impl Value {
    pub fn lox_eq(&self, rhs: &Value) -> Value {
        let b = match (self, rhs) {
            (Value::Num(a), Value::Num(b)) if a == b => true,
            (Value::Bool(a), Value::Bool(b)) if a == b => true,
            (Value::Nil, Value::Nil) => true,
            (Value::Str(a), Value::Str(b)) => a == b,
            _ => false,
        };
        Value::Bool(b)
    }
}

#[inline]
pub fn greater(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Bool(a > b),
        _ => panic!("Operands to > must be numbers"),
    }
}

#[inline]
pub fn less(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Bool(a < b),
        _ => panic!("Operands to < must be numbers"),
    }
}

#[inline]
pub fn negate(v: Value) -> Value {
    match v {
        Value::Num(x) => Value::Num(-x),
        _ => panic!("Can only negate numbers"),
    }
}

#[inline]
pub fn is_falsey(v: Value) -> Value {
    let b = match v {
        Value::Nil => true,
        Value::Bool(false) => true,
        _ => false,
    };
    Value::Bool(b)
}

#[inline]
pub fn add(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
        (Value::Str(s1), Value::Str(s2)) => {
            let mut s = String::new();
            s.push_str(s1.as_ref());
            s.push_str(s2.as_ref());
            Value::Str(Rc::new(s))
        }
        _ => panic!("Can only add numbers and strings"),
    }
}

#[inline]
pub fn subtract(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
        _ => panic!("Can only subtract numbers"),
    }
}

#[inline]
pub fn multiply(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
        _ => panic!("Can only multiply numbers"),
    }
}

#[inline]
pub fn divide(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
        _ => panic!("Can only divide numbers"),
    }
}
