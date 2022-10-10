use crate::Value;
use std::time::{SystemTime, UNIX_EPOCH};

type NativeFn = fn(&mut [Value]) -> Value;

pub const ALL_NATIVE_FNS: [(&str, NativeFn); 1] = [("clock", clock)];

fn clock(_args: &mut [Value]) -> Value {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    let secs = since_the_epoch.as_secs();
    Value::Num(secs as f64)
}
