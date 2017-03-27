extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate futures;
extern crate tokio_service;
extern crate hyper;
extern crate core;

pub mod service;
pub mod parser;
mod resolve;

pub use parser::{Field, parse};
pub use resolve::{Resolvable, Resolve, ResolveError, Value};
