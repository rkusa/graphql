use std::{io, clone, error, fmt};
use std::sync::{Arc, Mutex};

use hyper;
use hyper::status::StatusCode;
use hyper::header::{ContentType, ContentLength};
use hyper::server::{Service, NewService, Request, Response};
use futures::{future, Future, BoxFuture, Stream};
use serde_json;
use ::{Resolvable, Resolve, Field, parser};
use resolve::resolve;

#[derive(Debug)]
pub enum Error {
    BadRequest(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::BadRequest(ref msg) => write!(f, "Bad Request: {}", msg),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::BadRequest(_) => "Bad Request",
        }
    }

    // TODO: keep track of cause? (e.g. serde error)
    // fn cause(&self) -> Option<&Error> {}
}

#[derive(Serialize, Deserialize)]
struct PostQuery {
    query: String,
    // TODO:
    // operationName
    // variables
}

struct Root(Arc<Mutex<Resolvable + Send>>);

impl Resolvable for Root {
    fn resolve(&self, field: &Field) -> Option<Resolve> {
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

fn handle_graphql_request<T>(buffer: &Vec<u8>, root: T) -> BoxFuture<Response, Error>
    where T: Resolvable
{
    let result = serde_json::from_slice::<PostQuery>(&buffer)
        .map_err(|_| Error::BadRequest("invalid json body".to_string()))
        .and_then(|query| {
            parser::parse(&query.query)
                .map_err(|err| {
                    println!("{:?}", err);
                    Error::BadRequest(err.as_str())
                })
        })
        .map(|ss| {
            resolve(&ss, &root)
                .map_err(|_| Error::BadRequest("failed resolving query".to_string()))
                .and_then(|result| {
                    serde_json::to_string_pretty(&result)
                        .map_err(|_| Error::BadRequest("failed creating query result".to_string()))
                })
                .map(|mut buffer| {
                    buffer += "\n";

                    Response::new()
                        .with_header(ContentLength(buffer.len() as u64))
                        .with_header(ContentType::json())
                        .with_body(buffer)

                })
                .boxed()
        });

    match result {
        Ok(f) => f,
        Err(e) => future::err(e).boxed(),
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
                    .or_else(|err| {
                        let res = match err {
                            Error::BadRequest(msg) => {
                                Response::new()
                                    .with_status(StatusCode::BadRequest)
                                    .with_body(msg)
                            },
                        };
                        future::ok(res)
                    })
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
