use chomp::parsers;
use chomp::prelude::*;
use chomp::ascii::{is_alphanumeric, is_alpha, is_digit, skip_whitespace};
use chomp::combinators::look_ahead;
use resolve::{Value as ResolveValue, Resolvable, ResolveError, resolve};
use futures::{future, Future, BoxFuture};
use std::str;

#[derive(PartialEq, Debug)]
pub enum Value {
    // [Const]Variable
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Null, // NullValue
          // EnumValue
          // ListValue[?Const]
          // ObjectValue[/Const]
}

#[derive(PartialEq, Debug)]
pub struct Field {
    pub alias: Option<String>,
    pub name: String,
    pub arguments: Option<Vec<(String, Value)>>,
    selection_set: Option<SelectionSet>,
}

impl Field {
    pub fn resolve<T>(&self, obj: &T) -> BoxFuture<ResolveValue, ResolveError>
        where T: Resolvable
    {
        match self.selection_set {
            Some(ref selection_set) => resolve(&selection_set, obj),
            None => future::err(ResolveError::NoSubFields).boxed(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct SelectionSet {
    pub fields: Vec<Field>,
}

// TODO: remove once new chomp released
#[inline]
pub fn skip_while1<I: Input, F>(i: I, mut f: F) -> SimpleResult<I, ()>
    where F: FnMut(I::Token) -> bool
{
    satisfy(i, &mut f).then(|i| skip_while(i, f))
}

fn name<I: U8Input>(i: I) -> SimpleResult<I, String> {
    #[inline]
    fn is_valid_name_first_character(c: u8) -> bool {
        c == b'_' || is_alpha(c)
    }

    #[inline]
    fn is_valid_name_character(c: u8) -> bool {
        c == b'_' || is_alphanumeric(c)
    }

    look_ahead(i, |i| satisfy(i, is_valid_name_first_character)).then(|i|
        take_while1(i, is_valid_name_character).bind(|i, name|
            // TODO: no unwrap
            i.ret(String::from_utf8(name.to_vec()).unwrap())))
}

pub fn number_value<I: U8Input>(i: I) -> SimpleResult<I, Value> {
    #[inline]
    fn is_non_zero_digit(c: u8) -> bool {
        c >= b'1' && c <= b'9'
    }

    // #[inline]
    // fn is_non_digit(c: u8) -> bool {
    //     !is_digit(c)
    // }

    #[inline]
    fn negative_sign<I: U8Input>(i: I) -> SimpleResult<I, u8> {
        option(i, |i| token(i, b'-'), b'+')
    }

    // #[inline]
    // fn zero<I: U8Input>(i: I) -> SimpleResult<I, ()> {
    //     token(i, b'0').bind(|i, zero| {
    //                             or(i,
    //                                |i| look_ahead(i, |i| satisfy(i, is_non_digit).map(|_| ())),
    //                                |i| eof(i))

    //                         })
    // }

    #[inline]
    fn integer_part<I: U8Input>(i: I) -> SimpleResult<I, ()> {
        or(i,
           |i| token(i, b'0').map(|_| ()),
           |i| skip_while1(i, is_non_zero_digit).then(|i| skip_while(i, is_digit)))
    }

    #[inline]
    fn fraction_part<I: U8Input>(i: I) -> SimpleResult<I, bool> {
        option(i,
               |i| token(i, b'.').then(|i| skip_while1(i, is_digit)).map(|_| true),
               false)
    }

    #[inline]
    fn exponent_indicator<I: U8Input>(i: I) -> SimpleResult<I, ()> {
        or(i, |i| token(i, b'e'), |i| token(i, b'E')).map(|_| ())
    }

    #[inline]
    fn sign<I: U8Input>(i: I) -> SimpleResult<I, ()> {
        option(i, |i| or(i, |i| token(i, b'+'), |i| token(i, b'-')), b'+').map(|_| ())
    }

    #[inline]
    fn exponent<I: U8Input>(i: I) -> SimpleResult<I, bool> {
        option(i,
               |i| {
                   exponent_indicator(i).then(sign).then(|i| skip_while1(i, is_digit)).map(|_| true)
               },
               false)

    }

    matched_by(i, |i| {
        negative_sign(i).then(integer_part).then(fraction_part)
        .bind(|i, has_fraction| exponent(i).map(|has_exponent|  has_fraction || has_exponent))
    })
            .map(|(b, is_float)| {
                let v = b.to_vec();
                // TODO: no unwrap
                let s = str::from_utf8(&v).unwrap();
                println!("{}", s);

                // TODO: no unwrap
                if is_float {
                    Value::Float(s.parse().unwrap())
                } else {
                    Value::Int(s.parse().unwrap())
                }
            })
}

pub fn boolean_value<I: U8Input>(i: I) -> SimpleResult<I, Value> {
    or(i,
       |i| string(i, b"true").map(|_| Value::Boolean(true)),
       |i| string(i, b"false").map(|_| Value::Boolean(false)))
}

fn value<I: U8Input>(i: I) -> SimpleResult<I, Value> {
    or(i, |i| number_value(i), |i| boolean_value(i))
}

fn argument<I: U8Input>(i: I) -> SimpleResult<I, (String, Value)> {
    parse!{i;
        let name = name();
        skip_whitespace();
        token(b':');
        skip_whitespace();
        let value = value();
        skip_whitespace();

        ret (name, value)
    }
}

fn arguments<I: U8Input>(i: I) -> SimpleResult<I, Option<Vec<(String, Value)>>> {
    parse!{i;
        token(b'(');
        skip_whitespace();
        let arguments = many(argument);
        skip_whitespace();
        token(b')');

        ret Some(arguments)
    }
}

fn alias<I: U8Input>(i: I) -> SimpleResult<I, Option<String>> {
    parse!{i;
        let name = name();
        skip_whitespace();
        token(b':');

        ret Some(name)
    }
}

fn field<I: U8Input>(i: I) -> SimpleResult<I, Field> {
    parse!{i;
        let alias = option(alias, None);
        skip_whitespace();
        let name = name();
        skip_whitespace();
        let arguments = option(arguments, None);
        skip_whitespace();
        let selection_set = option(selection_set, None);
        skip_whitespace();

        ret Field {
            alias: alias,
            name: name,
            arguments: arguments,
            selection_set: selection_set,
        }
    }
}

fn selection_set<I: U8Input>(i: I) -> SimpleResult<I, Option<SelectionSet>> {
    parse!{i;
        token(b'{');
        skip_whitespace();
        let fields = many(field);
        skip_whitespace();
        skip_whitespace();
        token(b'}');

        ret Some(SelectionSet {
            fields: fields,
        })
    }
}

fn query<I: U8Input>(i: I) -> SimpleResult<I, Option<SelectionSet>> {
    parse!{i;
        string(b"query");
        skip_whitespace();
        selection_set()
    }
}

pub fn parse(input: &[u8]) -> Result<SelectionSet, (&[u8], parsers::Error<u8>)> {
    match parse_only(query, input) {
        Ok(ss) => Ok(ss.unwrap()),
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod test {
    use parser::*;

    #[test]
    fn value_int() {
        assert_eq!(parse_only(value, b"42"), Ok(Value::Int(42)));
        assert_eq!(parse_only(value, b"-42"), Ok(Value::Int(-42)));
        assert_eq!(parse_only(value, b"0"), Ok(Value::Int(0)));
        assert_eq!(parse_only(value, b"-0"), Ok(Value::Int(0)));

        // skipping wrong number parts
        assert_eq!(parse_only(value, b"042"), Ok(Value::Int(0)));
        // assert_eq!(parse_only(value, b"042"), unexpected(b"042"));
        assert_eq!(parse_only(value, b"-042"), Ok(Value::Int(0)));
        // assert_eq!(parse_only(value, b"-042"), unexpected(b"042"));

        // TODO: test overflow
    }

    #[test]
    fn value_float() {
        assert_eq!(parse_only(value, b"1.0"), Ok(Value::Float(1.0)));
        assert_eq!(parse_only(value, b"0.0"), Ok(Value::Float(0.0)));
        assert_eq!(parse_only(value, b"0.04"), Ok(Value::Float(0.04)));

        assert_eq!(parse_only(value, b"4.2e5"), Ok(Value::Float(420000.0)));
        assert_eq!(parse_only(value, b"4.2E5"), Ok(Value::Float(420000.0)));
        assert_eq!(parse_only(value, b"4.2e+5"), Ok(Value::Float(420000.0)));
        assert_eq!(parse_only(value, b"4.2E+5"), Ok(Value::Float(420000.0)));
        assert_eq!(parse_only(value, b"4.2e-5"), Ok(Value::Float(0.000042)));
        assert_eq!(parse_only(value, b"4.2E-5"), Ok(Value::Float(0.000042)));
        assert_eq!(parse_only(value, b"42e+5"), Ok(Value::Float(4200000.0)));
        assert_eq!(parse_only(value, b"42E+5"), Ok(Value::Float(4200000.0)));
        assert_eq!(parse_only(value, b"42e-5"), Ok(Value::Float(0.00042)));
        assert_eq!(parse_only(value, b"42E-5"), Ok(Value::Float(0.00042)));

        // skipping wrong number parts
        assert_eq!(parse_only(value, b"42."), Ok(Value::Int(42)));
        assert_eq!(parse_only(value, b"42.0e"), Ok(Value::Float(42.0)));
        assert_eq!(parse_only(value, b"42.0E"), Ok(Value::Float(42.0)));
        assert_eq!(parse_only(value, b"42e+"), Ok(Value::Int(42)));
        assert_eq!(parse_only(value, b"42E-"), Ok(Value::Int(42)));

        // TODO: test overflow
    }

    #[test]
    fn value_boolean() {
        assert_eq!(parse_only(value, b"true"), Ok(Value::Boolean(true)));
        assert_eq!(parse_only(value, b"false"), Ok(Value::Boolean(false)));
    }

    // fn unexpected(input: &[u8]) -> Result<Value, (&[u8], parsers::Error<u8>)> {
    //     Err::<Value, (&[u8], parsers::Error<u8>)>((input, parsers::Error::unexpected()))
    // }
}