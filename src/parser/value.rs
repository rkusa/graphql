use std::collections::HashMap;
use serde_json::{Value as JsonValue};

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Null,
    Enum(String),
    List(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl From<i32> for Value {
    fn from(i: i32) -> Value {
        Value::Int(i)
    }
}

impl<'a> Into<Option<i32>> for &'a Value {
    fn into(self) -> Option<i32> {
        if let &Value::Int(i) = self {
            Some(i)
        } else {
            None
        }
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Value {
        Value::Float(f)
    }
}

impl<'a> Into<Option<f32>> for &'a Value {
    fn into(self) -> Option<f32> {
        if let &Value::Float(f) = self {
            Some(f)
        } else {
            None
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl<'a> Into<Option<bool>> for &'a Value {
    fn into(self) -> Option<bool> {
        if let &Value::Boolean(b) = self {
            Some(b)
        } else {
            None
        }
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl<'a> Into<Option<String>> for &'a Value {
    fn into(self) -> Option<String> {
        if let &Value::String(ref s) = self {
            Some(s.clone())
        } else {
            None
        }
    }
}

impl<'a> From<&'a str> for Value {
    fn from(s: &'a str) -> Value {
        Value::String(s.to_string())
    }
}

impl<'a> Into<Option<&'a str>> for &'a Value {
    fn into(self) -> Option<&'a str> {
        if let &Value::String(ref s) = self {
            Some(s)
        } else {
            None
        }
    }
}

impl<'a> From<&'a JsonValue> for Value {
    fn from(v: &'a JsonValue) -> Value {
        match v {
            &JsonValue::Null => Value::Null,
            &JsonValue::Bool(b) => Value::Boolean(b),
            &JsonValue::Number(ref n) => {
                if n.is_f64() {
                    // TODO as f64
                    // TODO unwrap?
                    Value::Float(n.as_f64().unwrap() as f32)
                } else {
                    Value::Int(n.as_i64().unwrap() as i32)
                }
            },
            &JsonValue::String(ref s) => Value::String(s.clone()),
            &JsonValue::Array(ref a) => Value::List(a.iter().map(Value::from).collect()),
            &JsonValue::Object(ref m) => {
                let mut new = HashMap::new();
                for (k, v) in m.iter() {
                    new.insert(k.clone(), Value::from(v));
                }
                Value::Object(new)
            },
        }
    }
}