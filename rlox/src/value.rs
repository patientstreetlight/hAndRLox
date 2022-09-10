use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
    Str(Rc<String>),
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