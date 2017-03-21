use chomp::parsers;
use chomp::prelude::*;
use chomp::ascii::{is_alphanumeric, is_alpha, is_digit, skip_whitespace, decimal};
use chomp::combinators::look_ahead;

#[derive(PartialEq, Debug)]
pub enum Value {
    // [Const]Variable
    Int(i32), // IntValue
    // FloatValue
    String(String), // StringValue
    // BooleanValue
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
    pub selection_set: Option<SelectionSet>,
}

#[derive(PartialEq, Debug)]
pub struct SelectionSet {
    pub fields: Vec<Field>,
}

fn is_valid_name_first_character(c: u8) -> bool {
    c == b'_' || is_alpha(c)
}

fn is_valid_name_character(c: u8) -> bool {
    c == b'_' || is_alphanumeric(c)
}

fn is_non_zero_digit(c: u8) -> bool {
    c >= b'1' && c <= b'9'
}

fn is_non_digit(c: u8) -> bool {
    !is_digit(c)
}

fn name<I: U8Input>(i: I) -> SimpleResult<I, String> {
    look_ahead(i, |i| satisfy(i, is_valid_name_first_character)).then(|i|
        take_while1(i, is_valid_name_character).bind(|i, name|
            // TODO: no unwrap
            i.ret(String::from_utf8(name.to_vec()).unwrap())))
}

fn zero<I: U8Input>(i: I) -> SimpleResult<I, i32> {
    parse!{i;
        token(b'0');
        look_ahead(|i| satisfy(i, is_non_digit));

        ret 0
    }
}

fn int_value<I: U8Input>(i: I) -> SimpleResult<I, i32> {
    option(i, |i| token(i, b'-').map(|c| Some(c)), None).bind(|i, sign| {
        or(i,
           |i| zero(i),
           |i| look_ahead(i, |i| satisfy(i, is_non_zero_digit)).then(|i| decimal(i)))
                .map(|int| if sign.is_some() { -int } else { int })
    })
}

fn value<I: U8Input>(i: I) -> SimpleResult<I, Value> {
    int_value(i).map(|v| Value::Int(v))

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