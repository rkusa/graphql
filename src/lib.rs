extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate futures;
extern crate tokio_service;
extern crate hyper;
extern crate core;
extern crate ctx;

pub mod service;
pub mod parser;
mod resolve;

pub use parser::{Field, parse, Value as ParserValue};
pub use resolve::{Resolvable, ResolveResult, ResolveValue, ResolveError, Value, resolve, Resolve};
