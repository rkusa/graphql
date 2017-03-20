use std::fmt;

use combine::*;
use combine::char::{digit, string, spaces};
use combine::primitives::ParseError;
use combine::range::take_while1;

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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Int(i) => write!(f, "{}", i),
            Value::String(ref s) => write!(f, "{}", s),
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Field<'a> {
    pub name: &'a str,
    pub arguments: Option<Vec<(&'a str, Value)>>,
    pub selection_set: Option<SelectionSet<'a>>,
}

impl<'a> fmt::Display for Field<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = self.name.to_string();

        match self.arguments {
            Some(ref arguments) => {
                s.push_str("( ");
                for argument in arguments {
                    s += format!("{}: {} ", argument.0, argument.1).as_str();
                }
                s.push_str(")");
            }
            None => {}
        };

        match self.selection_set {
            Some(ref selection_set) => {
                // s.push_str("( ");
                s += selection_set.to_string().as_str();
                // s.push_str(")");
            }
            None => {}
        };

        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Debug)]
pub struct SelectionSet<'a> {
    pub fields: Vec<Field<'a>>,
}

impl<'a> fmt::Display for SelectionSet<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // let mut s = String::new();
        // // write!(f, "\{\n");
        // for field in self.fields {
        //     s.push_str(field.to_string());
        //     s.push_str("\n");
        //     write!(f, "field");
        // }
        let content = self.fields.iter().fold(String::new(), |acc, ref arg| {
            acc + "  " + arg.to_string().as_str() + "\n"
        });
        write!(f, "{{\n{}}}", content)
    }
}

// fn end_of_line<I>(input: I) -> ParseResult<char, I>
//     where I: RangeStream<Item=char>
// {
//     crlf().or(newline()).expected("end of line").parse_stream(input)
// }

fn is_valid_name_character(c: char) -> bool {
    match c {
        '_' | 'a'...'z' | 'A'...'Z' | '0'...'9' => true,
        _ => false,
    }
}

fn name<'a>(input: &'a str) -> ParseResult<&'a str, &'a str> {
    not_followed_by(digit())
        .and(take_while1(is_valid_name_character))
        .map(|(_, name)| name)
        .expected("name")
        .parse_stream(input)
}

fn int_value(input: &str) -> ParseResult<i32, &str> {
    // if 0, not followed by further digets
    let digits =
        token('0').and(not_followed_by(digit())).map(|(_, _)| "0".to_string()).or(many1(digit()));

    (optional(token('-')), digits)
        .map(|(sign, v)| {
            let mut int = v.parse::<i32>().unwrap();
            if sign.is_some() {
                int = -int;
            }
            int
        })
        .expected("int_value")
        .parse_stream(input)
}

fn value(input: &str) -> ParseResult<Value, &str> {
    choice([
        // IntValue
        parser(int_value).map(|v| Value::Int(v)),
    ])
        // .map(|(_, name)| name)
        .expected("value")
        .parse_stream(input)
}

fn argument(input: &str) -> ParseResult<(&str, Value), &str> {
    (parser(name), spaces(), token(':'), spaces(), parser(value))
        .map(|(name, _, _, _, value)| (name, value))
        .expected("argument")
        .parse_stream(input)
}

fn arguments(input: &str) -> ParseResult<Vec<(&str, Value)>, &str> {
    (token('('), spaces(), many(parser(argument)), spaces(), token(')'))
        .map(|(_, _, arguments, _, _)| arguments)
        .expected("arguments")
        .parse_stream(input)
}

fn field<'a>(input: &'a str) -> ParseResult<Field<'a>, &'a str> {
    // Field: (Alias) Name (Arguments) (Directoves) (SelectionSet)
    // TODO:
    (parser(name), spaces(), optional(parser(arguments)), spaces(), optional(parser(selection_set)))
        .map(|(name, _, arguments, _, selection_set)| {
            Field {
                name: name,
                arguments: arguments,
                selection_set: selection_set,
            }
        })
        .expected("field")
        .parse_stream(input)
}

fn fields<'a>(input: &'a str) -> ParseResult<Vec<Field<'a>>, &'a str> {
    many(parser(field)).expected("fields").parse_stream(input)
}

fn selection_set<'a>(input: &'a str) -> ParseResult<SelectionSet<'a>, &'a str> {
    // SelectionSet
    (token('{'), spaces(), parser(fields), spaces(), token('}'))
        .map(|(_, _, fields, _, _)| SelectionSet { fields: fields })
        .expected("selection_set")
        .parse_stream(input)
}

pub fn parse(input: &str) -> Result<SelectionSet, ParseError<&str>> {
    let operation_type = string("query").or(string("mutation"));
    let mut query = (operation_type, spaces(), parser(selection_set)).map(|(_, _, ss)| ss);
    query.parse(input).map(|t| t.0)
}