extern crate hyper;
extern crate serde_json;
extern crate futures;
extern crate graphql;
extern crate futures_cpupool;

use hyper::server::Http;
use graphql::{Resolvable, Field, Resolve};
// use futures::{future, Future, BoxFuture};

struct User {
    id: i32,
    name: String,
}

impl Resolvable for User {
    fn resolve(&self, field: &Field) -> Option<Resolve> {
        match field.name.as_ref() {
            "id" => Some(self.id.into()),
            "name" => Some(self.name.to_string().into()),
            _ => None,
        }
    }
}

struct Root;

impl Resolvable for Root {
    fn resolve(&self, field: &Field) -> Option<Resolve> {
        match field.name.as_ref() {
            "user" => {
                let user = User {
                    id: 42,
                    name: "Markus".to_string(),
                };
                field.resolve(&user)
            }
            _ => None,
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