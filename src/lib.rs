extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;
extern crate futures;
extern crate tokio_service;
extern crate hyper;
#[macro_use] extern crate chomp;

use std::{fmt, io, str, error};

use tokio_service::Service;
use futures::{future, Future, BoxFuture};
pub use parser::{Field, parse};
use parser::{SelectionSet};
use serde::ser::{Serialize, Serializer, SerializeMap};
use serde_json::{Number};

pub mod service;
pub mod parser;

pub struct GraphQL;

impl Service for GraphQL {
    // These types must match the corresponding protocol types:
    type Request = String;
    type Response = String;

    // For non-streaming protocols, service errors are always io::Error
    type Error = io::Error;

    // The future for computing the response; box it for simplicity.
    type Future = BoxFuture<Self::Response, Self::Error>;

    // Produce a future for computing a response from a request.
    fn call(&self, req: Self::Request) -> Self::Future {
        // In this case, the response is immediate.
        future::ok(req).boxed()
    }
}

pub struct Resolve;

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

impl fmt::Display for Value {
    /// Serializes a json value into a string
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut wr = WriterFormatter {
            inner: f,
        };
        serde_json::to_writer_pretty(&mut wr, self).map_err(|_| fmt::Error)
    }
}

struct WriterFormatter<'a, 'b: 'a> {
    inner: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> io::Write for WriterFormatter<'a, 'b> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        fn io_error<E>(_: E) -> io::Error {
            // Value does not matter because fmt::Debug and fmt::Display impls
            // below just map it to fmt::Error
            io::Error::new(io::ErrorKind::Other, "fmt error")
        }
        let s = try!(str::from_utf8(buf).map_err(io_error));
        try!(self.inner.write_str(s).map_err(io_error));
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
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
    // TODO: the following could not be used, see https://github.com/alexcrichton/futures-rs/issues/285
    // future::join_all(selection_set.fields.iter().map(|field| { ... }))

    let mut fs = Vec::new();

    for field in &selection_set.fields {
        let field_name = field.name.to_string();
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
