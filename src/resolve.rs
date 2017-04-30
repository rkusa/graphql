use std::{fmt, str, error};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use futures::{future, Future, Stream};
use parser::{SelectionSet, Field, Value as ParserValue, FromGql, FromGqlError};
use serde::ser::{Serialize, Serializer, SerializeMap};
use serde_json::Number;
use ctx::Context;

pub enum ResolveValue {
    Now(Value),
    Async(Box<Future<Item = Value, Error = ResolveError>>),
}

pub type ResolveResult = Result<ResolveValue, ResolveError>;

pub trait Resolvable {
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult;
}

impl<T> Resolvable for Option<T>
    where T: Resolvable
{
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult {
        match self {
            &Some(ref v) => v.resolve(r, name),
            &None => Ok(ResolveValue::Now(Value::Null)),
        }
    }
}

pub struct Resolve<'a> {
    name: &'a str,
    ctx: Context,
    fields: Option<Rc<RefCell<Vec<Field>>>>,
    args: Option<&'a HashMap<String, ParserValue>>,
}

impl<'a> Resolve<'a> {
    pub fn context(&self) -> &Context {
        &self.ctx
    }

    pub fn none(&self) -> ResolveResult {
        Err(ResolveError::InvalidField(self.name.to_string()))
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

    pub fn arg<T>(&self, name: &str) -> Result<T, ResolveError>
        where T: FromGql
    {
        match self.args.and_then(|m| m.get(name)) {
            Some(v) => T::from_gql(v).map_err(|err| {
                match err {
                    FromGqlError::InvalidType => ResolveError::InvalidArgumentType(name.to_string()),
                }
            }),
            None => Err(ResolveError::ExpectedArgument(name.to_string()))
        }
    }

    pub fn value<T>(&self, val: T) -> ResolveResult
        where T: Into<Value>
    {
        Ok(ResolveValue::Now(val.into()))
    }

    pub fn resolve<'b, T, R>(&self, val: T) -> ResolveResult
        where T: Into<IntermediateResult<'b, R>>,
              R: Resolvable + 'static
    {
        Ok(ResolveValue::Async(match self.fields {
                                      Some(ref fields) => {
                                          val.into().resolve(self.ctx.clone(), fields.clone())
                                      }
                                      None => future::err(ResolveError::NoSubFields).boxed(),
                                  }))
    }

    pub fn future<'b, F, R, I>(&self, fut: F) -> ResolveResult
        where F: Future<Item = I, Error = ResolveError> + 'static,
              R: Resolvable + 'static,
              I: Into<IntermediateResult<'b, R>>
    {
        Ok(ResolveValue::Async(match self.fields {
            Some(ref fields) => {
                let ctx = self.ctx.clone();
                let fields = fields.clone();
                Box::new(fut.map(move |obj| obj.into().resolve(ctx, fields))
                             .flatten())
            }
            None => future::err(ResolveError::NoSubFields).boxed(),
        }))
    }

    pub fn stream<S, R>(&self, stream: S) -> ResolveResult
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
                Ok(ResolveValue::Async(Box::new(resolve)))
            }
            None => Ok(ResolveValue::Async(future::err(ResolveError::NoSubFields).boxed())),
        }
    }
}

// TODO: 'a vs 'static
pub enum IntermediateResult<'a, T: Resolvable + 'static> {
    Borrowed(&'a T),
    Owned(T),
    VecBorrowed(Vec<&'static T>),
    VecOwned(Vec<T>),
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
            IntermediateResult::VecOwned(vec) => {
                let fs = vec.into_iter().map(move |obj| {
                    resolve_inner(ctx.clone(), fields.clone(), &obj)
                });
                let resolve = future::join_all(fs).map(Value::Array);
                Box::new(resolve)
            }
            IntermediateResult::VecBorrowed(vec) => {
                let fs = vec.into_iter().map(move |obj| {
                    resolve_inner(ctx.clone(), fields.clone(), obj)
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
        IntermediateResult::VecOwned(vec)
    }
}

impl<'a, T> From<Vec<&'static T>> for IntermediateResult<'a, T>
    where T: Resolvable
{
    fn from(vec: Vec<&'static T>) -> IntermediateResult<'a, T> {
        IntermediateResult::VecBorrowed(vec)
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

impl From<u32> for Value {
    fn from(i: u32) -> Value {
        Value::Number(i.into())
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Value {
        Value::Number(i.into())
    }
}

impl From<u64> for Value {
    fn from(i: u64) -> Value {
        Value::Number(i.into())
    }
}

impl From<f32> for Value {
    fn from(i: f32) -> Value {
        // TODO: no default of 0
        Value::Number(Number::from_f64(i as f64).unwrap_or_else(|| 0.into()))
    }
}

impl From<f64> for Value {
    fn from(i: f64) -> Value {
        // TODO: no unwrap
        Value::Number(Number::from_f64(i).unwrap_or_else(|| 0.into()))
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

impl<'a> From<&'a str> for Value {
    fn from(s: &'a str) -> Value {
        Value::String(s.to_string())
    }
}

impl<T> From<Option<T>> for Value
    where T: Into<Value>
{
    fn from(v: Option<T>) -> Value {
        match v {
            Some(v) => v.into(),
            None => Value::Null,
        }
    }
}

impl<T> From<Vec<T>> for Value
    where T: Into<Value>
{
    fn from(v: Vec<T>) -> Value {
        Value::Array(v.into_iter().map(|v| v.into()).collect())
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
    ExpectedArgument(String),
    InvalidArgumentType(String),
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ResolveError::InvalidField(ref name) => write!(f, "Cannot query field {}", name),
            ResolveError::NoSubFields => write!(f, "Field does not have subfields"),
            ResolveError::ExpectedArgument(ref name) => write!(f, "Expected argument {} was not specified", name),
            ResolveError::InvalidArgumentType(ref name) => write!(f, "Argument {} has invalid type", name),
        }
    }
}

impl error::Error for ResolveError {
    fn description(&self) -> &str {
        match *self {
            ResolveError::InvalidField(_) => "Query has an invalid field",
            ResolveError::NoSubFields => "Resolver expected subfields where no subfields were specified",
            ResolveError::ExpectedArgument(_) => "Expected argument was not specified",
            ResolveError::InvalidArgumentType(_) => "Argument has invalid type",
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
            name: &field.name,
            ctx: ctx.clone(),
            fields: fields,
            args: field.arguments.as_ref(),
        };
        // let args = Arguments(field.arguments.as_ref());

        match root.resolve(ex, &field.name) {
            Ok(ResolveValue::Now(val)) => obj.push((field_name, val)),
            Ok(ResolveValue::Async(fut)) => {
                let i = obj.len();
                obj.push((field_name, Value::Null));
                fs.push(fut.map(move |val| (i, val)))
            }
            Err(err) => return future::err(err).boxed(),
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
    use ctx::background;

    let mut hm = HashMap::new();
    hm.insert("test".to_string(), ParserValue::Int(42));
    let resolve = Resolve {
        name: "test",
        ctx: background(),
        fields: None,
        args: Some(&hm),
    };
    assert_eq!(resolve.arg::<i32>("test").ok(), Some(42));
}
