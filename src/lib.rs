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

use std::io;

use tokio_service::Service;
use futures::{future, Future, BoxFuture};
pub use parser::{Field, parse};
pub use resolve::{Resolvable, ResolveError, Value};
use resolve::resolve;

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