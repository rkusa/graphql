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

#[derive(Debug, PartialEq)]
pub enum FromGqlError {
    InvalidType
}

pub trait FromGql: Sized {
    fn from_gql(value: &Value) -> Result<Self, FromGqlError>;
}

impl From<i32> for Value {
    fn from(i: i32) -> Value {
        Value::Int(i)
    }
}

impl FromGql for i32 {
    fn from_gql(value: &Value) -> Result<Self, FromGqlError> {
        if let &Value::Int(i) = value {
            Ok(i)
        } else {
            Err(FromGqlError::InvalidType)
        }
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Value {
        Value::Float(f)
    }
}

impl FromGql for f32 {
    fn from_gql(value: &Value) -> Result<Self, FromGqlError> {
        if let &Value::Float(f) = value {
            Ok(f)
        } else {
            Err(FromGqlError::InvalidType)
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl FromGql for bool {
    fn from_gql(value: &Value) -> Result<Self, FromGqlError> {
        if let &Value::Boolean(b) = value {
            Ok(b)
        } else {
            Err(FromGqlError::InvalidType)
        }
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl FromGql for String {
    fn from_gql(value: &Value) -> Result<Self, FromGqlError> {
        if let &Value::String(ref s) = value {
            Ok(s.clone())
        } else {
            Err(FromGqlError::InvalidType)
        }
    }
}

impl<'a> From<&'a str> for Value {
    fn from(s: &'a str) -> Value {
        Value::String(s.to_string())
    }
}

impl<T> From<Vec<T>> for Value
    where Value: From<T>
{
    fn from(v: Vec<T>) -> Value {
        Value::List(v.into_iter().map(|v| v.into()).collect())
    }
}

impl<T> FromGql for Vec<T>
    where T: FromGql
{
    fn from_gql(value: &Value) -> Result<Self, FromGqlError> {
        if let &Value::List(ref vec) = value {
            let mut result = Vec::with_capacity(vec.len());
            for v in vec.iter() {
                result.push(T::from_gql(v)?)
            }
            Ok(result)
        } else {
            Err(FromGqlError::InvalidType)
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