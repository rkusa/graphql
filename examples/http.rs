extern crate hyper;
extern crate serde_json;
extern crate futures;
extern crate graphql;
extern crate futures_cpupool;

use hyper::server::Http;
use graphql::{Resolvable, ResolveError, Value, Field};
use futures::{future, Future, BoxFuture};

struct User {
    id: i32,
    name: String,
}

impl Resolvable for User {
    fn resolve(&self, field: &Field) -> BoxFuture<Value, ResolveError> {
        let f = match field.name.as_ref() {
            "id" => future::ok(Value::Number(self.id.into())),
            "name" => future::ok(Value::String(self.name.to_string())),
            _ => future::err(ResolveError::InvalidField(field.name.to_string())),
        };
        f.boxed()
    }
}

struct Root;

impl Resolvable for Root {
    fn resolve(&self, field: &Field) -> BoxFuture<Value, ResolveError> {
        match field.name.as_ref() {
            "user" => {
                let user = User {
                    id: 42,
                    name: "Markus".to_string(),
                };
                field.resolve(&user)
            }
            _ => future::err(ResolveError::InvalidField(field.name.to_string())).boxed(),
        }
    }
}


fn main() {
    let socket = "127.0.0.1:3000".parse().unwrap();
    // let socket = ([127, 0, 0, 1], 3000).into();
    let _ = Http::new()
        .bind(&socket, graphql::service::GraphQL::new(Root))
        .map(|server| server.run())
        .map_err(|e| println!("Server failed to setup: {}", e));
}