#![feature(test)]

extern crate test;
extern crate graphql;

use test::Bencher;

use graphql::parser;

const TEST_STR: &'static str = r#"query {
  user (id: -123) {
      name
      id
  }
}"#;

#[bench]
fn bench_parse(b: &mut Bencher) {
    b.iter(|| { let _ = parser::parse(TEST_STR); });
}