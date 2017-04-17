extern crate hyper;
extern crate graphql;
extern crate ctx;

use hyper::server::Http;
use graphql::{Resolvable, ResolveResult, Resolve};

struct User {
    id: i32,
    name: String,
}

impl Resolvable for User {
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult {
        match name {
            "id" => r.value(self.id),
            "name" => r.value(&self.name),
            _ => r.none(),
        }
    }
}

#[derive(Clone)]
struct Root;

impl Resolvable for Root {
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult {
        match name {
            "user" => {
                let user = User {
                    id: 42,
                    name: "Markus".to_string(),
                };
                r.resolve(user)
                // field.resolve(ctx, &user)
            }
            _ => r.none(),
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