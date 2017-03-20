#![feature(test)]

extern crate test;
extern crate graphql;

use test::Bencher;

use graphql::parser;

const TEST_STR: &'static str = r#"query {bu
  user (id: -123) {
      name
      id
  }
}"#;

#[bench]
fn bench_parse(b: &mut Bencher) {
    let input = TEST_STR.as_bytes();
    b.iter(|| { let _ = parser::parse(input); });
}