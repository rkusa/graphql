use std::{fmt, str, error};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use futures::{future, Future, Stream};
use parser::{SelectionSet, Field, Value as ParserValue};
use serde::ser::{Serialize, Serializer, SerializeMap};
use serde_json::Number;
use ctx::Context;

pub trait Resolvable {
    fn resolve(&self, r: Resolve, name: &str, args: Arguments) -> Option<ResolveResult>;
}

pub struct Arguments<'a>(Option<&'a HashMap<String, ParserValue>>);

impl<'a> Arguments<'a> {
    pub fn get<T>(&self, name: &str) -> Option<T>
        where &'a ParserValue: Into<Option<T>>
    {
        self.0.and_then(|m| m.get(name).and_then(|v| v.into()))
    }
}

pub struct Resolve {
    ctx: Context,
    fields: Option<Rc<RefCell<Vec<Field>>>>,
}

impl Resolve {
    pub fn value<T>(&self, val: T) -> Option<ResolveResult>
        where T: Into<Value>
    {
        Some(ResolveResult::Now(val.into()))
    }

    pub fn has_field(&self, name: &str) -> bool {
        match self.fields {
            Some(ref fields) => {
                // TODO: better contains search
                for field in fields.borrow().iter() {
                    if field.name == name  {
                        return true
                    }
                }

                false
            }
            None => false
        }
    }

    pub fn context(&self) -> &Context {
        &self.ctx
    }

    pub fn resolve<'a, T, R>(&self, val: T) -> Option<ResolveResult>
        where T: Into<IntermediateResult<'a, R>>,
              R: Resolvable + 'static
    {
        Some(ResolveResult::Async(match self.fields {
                                      Some(ref fields) => {
                                          val.into().resolve(self.ctx.clone(), fields.clone())
                                      }
                                      None => future::err(ResolveError::NoSubFields).boxed(),
                                  }))
    }

    pub fn future<'a, F, R, I>(&self, fut: F) -> Option<ResolveResult>
        where F: Future<Item = I, Error = ResolveError> + 'static,
              R: Resolvable + 'static,
              I: Into<IntermediateResult<'a, R>>
    {
        Some(ResolveResult::Async(match self.fields {
            Some(ref fields) => {
                let ctx = self.ctx.clone();
                let fields = fields.clone();
                Box::new(fut.map(move |obj| obj.into().resolve(ctx, fields))
                             .flatten())
            }
            None => future::err(ResolveError::NoSubFields).boxed(),
        }))
    }

    pub fn stream<S, R>(&self, stream: S) -> Option<ResolveResult>
        where S: Stream<Item = R, Error = ResolveError> + 'static,
              R: Resolvable + 'static
    {
        match self.fields {
            Some(ref fields) => {
                let ctx = self.ctx.clone();
                let fields = fields.clone();
                let resolve = stream
                    .map(move |obj| resolve_inner(ctx.clone(), fields.clone(), &obj))
                    .collect()
                    .and_then(|fs| future::join_all(fs).map(Value::Array));
                Some(ResolveResult::Async(Box::new(resolve)))
            }
            None => Some(ResolveResult::Async(future::err(ResolveError::NoSubFields).boxed())),
        }
    }
}

pub enum ResolveResult {
    Now(Value),
    Async(Box<Future<Item = Value, Error = ResolveError>>),
}

pub enum IntermediateResult<'a, T: Resolvable + 'a> {
    Borrowed(&'a T),
    Owned(T),
    Vec(Vec<T>),
}

impl<'a, T> IntermediateResult<'a, T>
    where T: Resolvable + 'static
{
    fn resolve(self,
               ctx: Context,
               fields: Rc<RefCell<Vec<Field>>>)
               -> Box<Future<Item = Value, Error = ResolveError>> {
        match self {
            IntermediateResult::Borrowed(obj) => resolve_inner(ctx, fields, obj),
            IntermediateResult::Owned(ref obj) => resolve_inner(ctx, fields, obj),
            IntermediateResult::Vec(vec) => {
                let fs = vec.into_iter().map(move |obj| {
                    resolve_inner(ctx.clone(), fields.clone(), &obj)
                });
                let resolve = future::join_all(fs).map(Value::Array);
                Box::new(resolve)
            }
        }
    }
}

impl<'a, T> From<T> for IntermediateResult<'a, T>
    where T: Resolvable + 'a
{
    fn from(r: T) -> IntermediateResult<'a, T> {
        IntermediateResult::Owned(r)
    }
}

impl<'a, T> From<&'a T> for IntermediateResult<'a, T>
    where T: Resolvable + 'a
{
    fn from(r: &'a T) -> IntermediateResult<'a, T> {
        IntermediateResult::Borrowed(r)
    }
}

impl<'a, T> From<Vec<T>> for IntermediateResult<'a, T>
    where T: Resolvable
{
    fn from(vec: Vec<T>) -> IntermediateResult<'a, T> {
        IntermediateResult::Vec(vec)
    }
}

#[derive(Debug)]
pub enum Value {
    Null,
    // Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),

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

impl<'a> From<&'a String> for Value {
    fn from(s: &'a String) -> Value {
        Value::String(s.clone())
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
            Value::Array(ref arr) => serializer.collect_seq(arr),
            Value::Object(ref obj) => {
                let mut map = serializer.serialize_map(Some(obj.len()))?;
                for kv in obj {
                    map.serialize_entry(&kv.0, &kv.1)?;
                }
                map.end()
            }
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
            ResolveError::InvalidField(ref name) => write!(f, "Cannot query field {}", name),
            ResolveError::NoSubFields => write!(f, "Field does not have subfields",),
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


pub fn resolve<T>(ctx: Context,
                  selection_set: SelectionSet,
                  root: &T)
                  -> Box<Future<Item = Value, Error = ResolveError>>
    where T: Resolvable
{
    resolve_inner(ctx, selection_set.fields, root)
}

pub fn resolve_inner<T>(ctx: Context,
                        fields: Rc<RefCell<Vec<Field>>>,
                        root: &T)
                        -> Box<Future<Item = Value, Error = ResolveError>>
    where T: Resolvable
{
    // NOTICE: the following could not be used:
    //      future::join_all(selection_set.fields.iter().map(|field| { ... }))
    // see https://github.com/alexcrichton/futures-rs/issues/285

    let mut fs = Vec::new();
    let mut obj = Vec::new();

    for field in fields.borrow().iter() {
        let field_name = match field.alias {
            Some(ref alias) => alias.clone(),
            None => field.name.clone(),
        };

        let fields = field.selection_set.as_ref().map(|ss| ss.fields.clone());
        let ex = Resolve {
            ctx: ctx.clone(),
            fields: fields,
        };
        let args = Arguments(field.arguments.as_ref());

        match root.resolve(ex, &field.name, args) {
            Some(ResolveResult::Now(val)) => obj.push((field_name, val)),
            Some(ResolveResult::Async(fut)) => {
                let i = obj.len();
                obj.push((field_name, Value::Null));
                fs.push(fut.map(move |val| (i, val)))
            }
            None => return future::err(ResolveError::InvalidField(field_name)).boxed(),
        }
    }

    let fut = future::join_all(fs).map(|values| {
                                           for value in values {
                                               obj[value.0].1 = value.1
                                           }
                                           Value::Object(obj)
                                       });

    Box::new(fut)
}

#[test]
fn test_arguments_get() {
    let mut hm = HashMap::new();
    hm.insert("test".to_string(), ParserValue::Int(42));
    let args = Arguments(Some(&hm));
    assert_eq!(args.get::<i32>("test"), Some(42));
}
