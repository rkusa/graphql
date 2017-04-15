#![feature(test)]

extern crate test;
extern crate graphql;
extern crate serde_json;

use test::Bencher;
use graphql::parser;
use serde_json::map::Map;

const TEST_STR: &'static str = r#"query ($id: Int = 42) {
  user (id: $id) {
      name
      id
  }
}"#;

#[bench]
fn bench_parse(b: &mut Bencher) {
    let vars = Map::new();
    b.iter(|| { let _ = parser::parse(TEST_STR, &vars); });
}