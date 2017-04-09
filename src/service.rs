use std::{io, error, fmt};
use hyper;
use hyper::status::StatusCode;
use hyper::header::{ContentType, ContentLength};
use hyper::server::{Service, NewService, Request, Response};
use futures::{future, Future, Stream};
use serde_json;
use {Resolvable, parser};
use resolve::resolve;
use ctx;

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

pub struct GraphQL<R>
    where R: Resolvable + Clone + 'static
{
    root: R,
}

impl<R> GraphQL<R>
    where R: Resolvable + Clone + 'static
{
    pub fn new(root: R) -> Self {
        return GraphQL { root: root };
    }
}

fn handle_graphql_request<T>(buffer: &Vec<u8>,
                             root: T)
                             -> Box<Future<Item = Response, Error = Error>>
    where T: Resolvable
{
    let result = serde_json::from_slice::<PostQuery>(&buffer)
        .map_err(|_| Error::BadRequest("invalid json body".to_string()))
        .and_then(|query| {
                      parser::parse(&query.query).map_err(|err| {
                                                              println!("{:?}", err);
                                                              Error::BadRequest(err.as_str())
                                                          })

                      .map(|ss| {
                          let result = resolve(ctx::background(), ss, &root)
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

                              });

                           Box::new(result)
                      })
                  });

    match result {
        Ok(f) => f,
        Err(e) => future::err(e).boxed(),
    }
}

impl<R> Service for GraphQL<R>
    where R: Resolvable + Clone + 'static
{
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error>>;

    fn call(&self, req: Self::Request) -> Self::Future {
        let root = self.root.clone();
        let (method, _, _, headers, body) = req.deconstruct();

        if method != hyper::Post {
            let res = Response::new().with_status(StatusCode::MethodNotAllowed);
            return future::finished(res).boxed();
        }

        if headers.get() != Some(&ContentType::json()) {
            let res = Response::new().with_status(StatusCode::UnsupportedMediaType);
            return future::finished(res).boxed();
        }

        let resp = body.fold(vec![], |mut body, chunk| {
                body.extend_from_slice(&chunk);
                Ok::<_, hyper::Error>(body)
            })
            .and_then(|buffer| {
                handle_graphql_request(&buffer, root).or_else(|err| {
                    let res = match err {
                        Error::BadRequest(msg) => {
                            Response::new()
                                .with_status(StatusCode::BadRequest)
                                .with_body(msg)
                        }
                    };
                    future::ok(res)
                })
            });

        Box::new(resp)
    }
}

impl<R> NewService for GraphQL<R>
    where R: Resolvable + Clone + 'static
{
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Instance = GraphQL<R>;

    fn new_service(&self) -> Result<Self::Instance, io::Error> {
        Ok(GraphQL { root: self.root.clone() })
    }
}
