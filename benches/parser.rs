#![feature(test)]

extern crate test;
extern crate graphql;

use test::Bencher;

use graphql::parser;
use graphql::parser2;

const TEST_STR: &'static str = r#"query {bu
  user (id: -123) {
      name
      id
  }
}"#;

#[bench]
fn bench_parse1(b: &mut Bencher) {
    b.iter(|| { let _ = parser::parse(TEST_STR); });
}

#[bench]
fn bench_parse2(b: &mut Bencher) {
    let input = TEST_STR.as_bytes();
    b.iter(|| { let _ = parser2::parse(input); });
}