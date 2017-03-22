use std::{fmt, str, error};

use futures::{future, Future, BoxFuture};
use parser::{SelectionSet};
pub use parser::{Field, parse};
use serde::ser::{Serialize, Serializer, SerializeMap};
use serde_json::{Number};


pub trait Resolvable {
    fn resolve(&self, field: &Field) -> BoxFuture<Value, ResolveError>;
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

pub enum Value {
    // Null,
    // Bool(bool),
    Number(Number),
    String(String),
    // Array(Vec<Value>),

    // a vector is used to preserve order of insertion (order of fields in the query)
    Object(Vec<(String, Value)>),
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
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

pub fn resolve<T>(selection_set: &SelectionSet, root: &T) -> BoxFuture<Value, ResolveError>
    where T: Resolvable
{
    // NOTICE: the following could not be used:
    //      future::join_all(selection_set.fields.iter().map(|field| { ... }))
    // see https://github.com/alexcrichton/futures-rs/issues/285

    let mut fs = Vec::new();

    for field in &selection_set.fields {
        let field_name = match field.alias {
            Some(ref alias) => alias.clone(),
            None => field.name.clone(),
        };
        let f = root.resolve(&field).map(move |val| {
            (field_name, val)
        });

        fs.push(f);
    }

    future::join_all(fs).map(|values| {
        let mut obj = Vec::new();
        for value in values {
            obj.push((value.0, value.1));
        };
        Value::Object(obj)
    }).boxed()
}
