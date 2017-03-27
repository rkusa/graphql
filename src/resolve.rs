use std::{fmt, str, error};

use futures::{future, Future, BoxFuture};
use parser::{SelectionSet, Field};
use serde::ser::{Serialize, Serializer, SerializeMap};
use serde_json::{Number};

pub trait Resolvable {
    fn resolve(&self, field: &Field) -> Option<Resolve>;
}

pub enum Resolve {
    Now(Value),
    Async(BoxFuture<Value, ResolveError>),
}

impl From<i32> for Resolve {
    fn from(i: i32) -> Resolve {
        Resolve::Now(i.into())
    }
}

impl From<String> for Resolve {
    fn from(s: String) -> Resolve {
        Resolve::Now(s.into())
    }
}

#[derive(Debug)]
pub enum Value {
    Null,
    // Bool(bool),
    Number(Number),
    String(String),
    // Array(Vec<Value>),

    // a vector is used to preserve order of insertion (order of fields in the query)
    Object(Vec<(String, Value)>),
}

impl From<i32> for Value {
    fn from(i: i32) -> Value {
        Value::Number(i.into())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            Value::Null => serializer.serialize_none(),
            Value::Number(ref nr) => nr.serialize(serializer),
            Value::String(ref s) => serializer.serialize_str(&s),
            Value::Object(ref obj) => {
                let mut map = serializer.serialize_map(Some(obj.len()))?;
                for kv in obj {
                    map.serialize_entry(&kv.0, &kv.1)?;
                }
                map.end()
            },
        }
    }
}

#[derive(Debug)]
pub enum ResolveError {
    InvalidField(String),
    NoSubFields,
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ResolveError::InvalidField(ref name) =>
                write!(f, "Cannot query field {}", name),
            ResolveError::NoSubFields =>
                write!(f, "Field does not have subfields",),
        }
    }
}

impl error::Error for ResolveError {
    fn description(&self) -> &str {
        match *self {
            ResolveError::InvalidField(_) => "Query has an invalid field",
            ResolveError::NoSubFields => "Resolver expected subfields where no subfields were specified",
        }
    }
}

pub fn resolve<T>(selection_set: &SelectionSet, root: &T) -> BoxFuture<Value, ResolveError>
    where T: Resolvable
{
    // NOTICE: the following could not be used:
    //      future::join_all(selection_set.fields.iter().map(|field| { ... }))
    // see https://github.com/alexcrichton/futures-rs/issues/285

    let mut fs = Vec::new();
    let mut obj = Vec::new();

    for field in &selection_set.fields {
        let field_name = match field.alias {
            Some(ref alias) => alias.clone(),
            None => field.name.clone(),
        };

        match root.resolve(&field) {
            Some(Resolve::Now(val)) => obj.push((field_name, val)),
            Some(Resolve::Async(fut)) => {
                let i = obj.len();
                obj.push((field_name, Value::Null));
                fs.push(fut.map(move |val| {
                    (i, val)
                }))
            },
            None => return future::err(ResolveError::InvalidField(field_name)).boxed(),
        }
    }

    future::join_all(fs).map(|values| {
        for value in values {
            obj[value.0].1 = value.1
        };
        Value::Object(obj)
    }).boxed()
}
