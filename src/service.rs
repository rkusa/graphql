use std::{io, clone};
use std::sync::{Arc, Mutex};

use hyper;
use hyper::status::StatusCode;
use hyper::header::{ContentType, ContentLength};
use hyper::server::{Service, NewService, Request, Response};
use futures::{future, Future, BoxFuture, Stream};
use serde_json;
use ::{Resolvable, resolve, Field, Value, ResolveError, parser};

#[derive(Serialize, Deserialize)]
struct PostQuery {
    query: String,
    // TODO:
    // operationName
    // variables
}

struct Root(Arc<Mutex<Resolvable + Send>>);

impl Resolvable for Root {
    fn resolve(&self, field: &Field) -> BoxFuture<Value, ResolveError> {
        let root = self.0.lock().unwrap();
        root.resolve(field)
    }
}

impl clone::Clone for Root {
    fn clone(&self) -> Self { Root(self.0.clone()) }
}

pub struct GraphQL {
  root: Root,
}

impl GraphQL {
  pub fn new<T>(root: T) -> Self
    where T: 'static + Resolvable + Send
{
    return GraphQL{
      root: Root(Arc::new(Mutex::new(root))),
    }
  }
}

fn handle_graphql_request<T>(buffer: &Vec<u8>, root: T) -> BoxFuture<Response, hyper::Error>
    where T: Resolvable
{
    match serde_json::from_slice::<PostQuery>(&buffer) {
        Ok(query) => {
            match parser::parse(&query.query) {
                Ok(ss) => {
                    // print!("Success:\n{}", ss);

                    resolve(&ss, &root)
                        .map(|result| {
                            match serde_json::to_string_pretty(&result) {
                                Ok(mut buffer) => {
                                    buffer += "\n";

                                    Response::new()
                                        .with_header(ContentLength(buffer.len() as u64))
                                        .with_header(ContentType::json())
                                        .with_body(buffer)
                                }
                                Err(_) => {
                                    Response::new().with_status(StatusCode::BadRequest)
                                }
                            }

                        })
                        .or_else(|_| {
                            future::ok(Response::new().with_status(StatusCode::BadRequest))
                        })
                        .boxed()
                }
                Err(_) => {
                    future::ok(Response::new().with_status(StatusCode::BadRequest)).boxed()
                }
            }
        }
        Err(_) => {
            future::ok(Response::new().with_status(StatusCode::BadRequest)).boxed()
        }
    }
}

impl Service for GraphQL {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = BoxFuture<Self::Response, Self::Error>;

    fn call(&self, req: Self::Request) -> Self::Future {
        let root = self.root.clone();
        let (method, _, _, headers, body) = req.deconstruct();

        if method != hyper::Post {
            let res = Response::new()
                .with_status(StatusCode::MethodNotAllowed);
            return future::finished(res).boxed()
        }

        if headers.get() != Some(&ContentType::json()) {
            let res = Response::new()
                .with_status(StatusCode::UnsupportedMediaType);
            return future::finished(res).boxed()
        }

        body.fold(vec![], |mut body, chunk| {
                body.extend_from_slice(&chunk);
                Ok::<_, hyper::Error>(body)
            })
            .and_then(|buffer| {
                handle_graphql_request(&buffer, root)
            })
            .boxed()
    }
}

impl NewService for GraphQL {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Instance = GraphQL;

    fn new_service(&self) -> Result<Self::Instance, io::Error> {
        Ok(GraphQL { root: Root(self.root.0.clone()) })
    }
}
