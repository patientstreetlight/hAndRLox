use crate::Chunk;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
    Str(Gc<String>),
    Function(Gc<Function>),
    NativeFn(Gc<NativeFn>),
    Closure(Gc<Closure>),
    Class(Gc<Class>),
    Instance(Gc<Instance>),
}

pub struct Instance {
    pub class: Gc<Class>,
    // TODO: will the keys by String, or Gc<String>?
    fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Gc<Class>) -> Instance {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn set_property(&mut self, property: &str, val: Value) {
        self.fields.insert(property.to_owned(), val);
    }

    pub fn get_property(&self, property: &str) -> Value {
        *self.fields.get(property).expect("undefined property")
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instance")
            .field("class", &self.class)
            .field("fields", &self.fields)
            .finish()
    }
}

pub struct Class {
    pub name: Gc<String>,
}

impl core::fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &*self.name)
    }
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
    pub function: Gc<Function>,
    pub upvalues: Vec<UpValueRef>,
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
            Self::Str(s) => s.fmt(f),
            Self::Function(function) => match &function.name {
                None => write!(f, "<script>"),
                Some(name) => write!(f, "<fn {}>", name),
            },
            Self::NativeFn(function) => write!(f, "<native fn {}>", function.name),
            Self::Closure(_closure) => write!(f, "<closure>"),
            Self::Class(class) => write!(f, "{}", &*class.name),
            Self::Instance(instance) => write!(f, "<{} instance>", &*instance.class.name),
        }
    }
}

impl Value {
    pub fn lox_eq(&self, rhs: &Value) -> Value {
        let b = match (self, rhs) {
            (Value::Num(a), Value::Num(b)) if a == b => true,
            (Value::Bool(a), Value::Bool(b)) if a == b => true,
            (Value::Nil, Value::Nil) => true,
            (Value::Str(a), Value::Str(b)) => a.ref_equals(*b) || str_equals(a, b),
            (Value::Function(a), Value::Function(b)) => a.ref_equals(*b),
            (Value::Closure(a), Value::Closure(b)) => a.ref_equals(*b),
            (Value::NativeFn(a), Value::NativeFn(b)) => a.ref_equals(*b),
            _ => false,
        };
        Value::Bool(b)
    }
}

#[inline]
fn str_equals(a: &str, b: &str) -> bool {
    a == b
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
pub fn add(a: Value, b: Value, allocator: &mut GcAllocator) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
        (Value::Str(s1), Value::Str(s2)) => {
            let mut s = String::with_capacity(s1.len() + s2.len());
            s.push_str(s1.as_str());
            s.push_str(s2.as_str());
            let s = allocator.alloc_constant(s);
            Value::Str(s)
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

#[derive(Debug)]
pub struct Gc<T> {
    obj: *mut T,
}

impl<T> Gc<T> {
    fn ref_equals(self, rhs: Gc<T>) -> bool {
        std::ptr::eq(self.obj, rhs.obj)
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.obj }
    }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.obj }
    }
}

// Can't auto-derive Copy and Clone for Gc<T> since the auto-derive
// only implements Copy/Clone when T implements Copy/Clone, but that
// restriction is not necessary for Gc<T> since it's just copying
// raw pointers.
impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Gc<T> {}

pub struct GcAllocator {
    // TODO: track all created objects which have not yet been freed.
}

impl GcAllocator {
    pub fn new() -> GcAllocator {
        GcAllocator {}
    }

    pub fn alloc_constant<T>(&mut self, val: T) -> Gc<T> {
        let ptr = Box::leak(Box::new(val));
        let ptr = ptr as *mut T;
        Gc { obj: ptr }
    }

    pub fn alloc<T, I>(&mut self, val: T, roots: I) -> Gc<T>
    where
        I: IntoIterator<Item = Value>,
    {
        // TODO: conditionally run a collection.
        self.alloc_constant(val)
    }

    fn collect<I>(&mut self, roots: I)
    where
        I: IntoIterator<Item = Value>,
    {
        // Roots are:
        // - all Gc values on the stack
        // - all Gc global variables
        // - closures in call stack frames
        // - the open upvalue list would be as well if they were GC'd objects
        todo!();
    }
}

// TODO: Dropping GcAllocator should free all outstanding objects.
