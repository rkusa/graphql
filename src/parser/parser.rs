use std::collections::HashMap;

use super::lexer::{Lexer, TokenKind, LexerError};
use {Resolvable, Resolve, ResolveError};
use resolve::resolve;
use futures::{future, Future};
use ctx::Context;

#[derive(PartialEq, Debug)]
pub enum Value {
    Variable(String, Option<Box<Value>>),
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Null,
    Enum(String),
    List(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl From<i32> for Value {
    fn from(i: i32) -> Value {
        Value::Int(i)
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Value {
        Value::Float(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(s: &'a str) -> Value {
        Value::String(s.to_string())
    }
}

#[derive(PartialEq, Debug)]
pub struct Field {
    pub alias: Option<String>,
    pub name: String,
    pub arguments: Option<HashMap<String, Value>>,
    selection_set: Option<SelectionSet>,
}

impl Field {
    pub fn resolve<T>(&mut self, ctx: Context, obj: &T) -> Option<Resolve>
        where T: Resolvable
    {
        match self.selection_set.take() {
            Some(selection_set) => Some(Resolve::Async(resolve(ctx, selection_set, obj))),
            None => Some(Resolve::Async(future::err(ResolveError::NoSubFields).boxed())),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct SelectionSet {
    pub fields: Vec<Field>,
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    lexer_error: Option<LexerError>,
    peeked: Option<TokenKind<'a>>,
    pos: usize,
}

#[derive(Debug, PartialEq)]
pub struct ParserError(pub ErrorKind, pub usize);

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    ExpectedToken,
    UnexpectedToken,
    LexerError(LexerError),
    UnexpectedEnd,
    InvalidQuery,
}

impl ParserError {
    pub fn as_str(&self) -> String {
        match self.0 {
            ErrorKind::ExpectedToken => format!("expected token at {}", self.1),
            ErrorKind::UnexpectedToken => format!("unexpected token at {}", self.1),
            ErrorKind::LexerError(_) => format!("syntax error at {}", self.1),
            ErrorKind::UnexpectedEnd => format!("unexpected end at {}", self.1),
            ErrorKind::InvalidQuery => format!("invalid query at {}", self.1),
        }
    }
}

impl From<LexerError> for ErrorKind {
    fn from(err: LexerError) -> ErrorKind {
        ErrorKind::LexerError(err)
    }
}

pub fn parse(src: &str) -> Result<SelectionSet, ParserError> {
    let mut parser = Parser::new(src);
    parser.document()
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Parser {
        let lexer = Lexer::new(src);
        Parser {
            lexer: lexer,
            lexer_error: None,
            peeked: None,
            pos: 0,
        }
    }

    #[inline]
    fn next_token(&mut self) -> Option<TokenKind<'a>> {
        match self.peeked {
            Some(_) => self.peeked.take(),
            None => {
                match self.lexer.scan() {
                    Ok(token) => {
                        self.pos = token.1;
                        Some(token.0)
                    }
                    Err(err) => {
                        self.lexer_error = Some(err);
                        None
                    }
                }
            }
        }
    }

    #[inline]
    fn peek_token(&mut self) -> Option<&TokenKind<'a>> {
        if self.peeked.is_none() {
            self.peeked = match self.lexer.scan() {
                Ok(token) => {
                    self.pos = token.1;
                    Some(token.0)
                }
                Err(err) => {
                    self.lexer_error = Some(err);
                    None
                }
            }
        }
        match self.peeked {
            Some(ref value) => Some(value),
            None => None,
        }
    }

    fn document(&mut self) -> Result<SelectionSet, ParserError> {
        // TODO: (OperationDefinition | FragmentDefinition)+

        self.operation()
            .map_err(|err| if let Some(err) = self.lexer_error.take() {
                         let pos = err.1;
                         ParserError(err.into(), pos)
                     } else {
                         ParserError(err, self.pos)
                     })
    }

    fn operation(&mut self) -> Result<SelectionSet, ErrorKind> {
        // TODO: SelectionSet |
        //       (("query" | "mutation") [Name] [VariableDefinitions] [Directives] SelectionSet)

        if let Some(&TokenKind::Name(operation)) = self.peek_token() {
            self.next_token(); // consume name

            if operation != "query" && operation != "mutation" {
                return Err(ErrorKind::InvalidQuery);
            }
        }

        match self.selection_set() {
            Ok(Some(ss)) => Ok(ss),
            Ok(None) => Err(ErrorKind::InvalidQuery),
            Err(err) => Err(err),
        }
    }

    fn selection_set(&mut self) -> Result<Option<SelectionSet>, ErrorKind> {
        // TODO: "{" (Field | FragmentSpread | InlineFragment)+ "}"

        if let Some(&TokenKind::LeftBrace) = self.peek_token() {
            self.next_token(); // consume left brace

            let mut fields = vec![];
            loop {
                match self.field() {
                    Ok(Some(field)) => fields.push(field),
                    Ok(None) => break,
                    Err(err) => return Err(err),
                }
            }

            self.expect(TokenKind::RightBrace)?;

            Ok(Some(SelectionSet { fields: fields }))
        } else {
            Ok(None)
        }
    }

    fn field(&mut self) -> Result<Option<Field>, ErrorKind> {
        // TODO: [Alias] Name [Arguments] [Directives] [SelectionSet]

        if let Some(&TokenKind::Name(n)) = self.peek_token() {
            self.next_token(); // consume name

            let mut name = n.to_string();
            let mut alias = None;

            if let Some(&TokenKind::Colon) = self.peek_token() {
                self.next_token(); // consume colon

                match self.next_token() {
                    Some(TokenKind::Name(a)) => {
                        alias = Some(name);
                        name = a.to_string()
                    }
                    Some(_) => return Err(ErrorKind::UnexpectedToken),
                    None => return Err(ErrorKind::InvalidQuery),
                }
            }

            let arguments = self.arguments()?;
            let selection_set = self.selection_set()?;

            Ok(Some(Field {
                        alias: alias,
                        name: name.to_string(),
                        arguments: arguments,
                        selection_set: selection_set,
                    }))
        } else {
            Ok(None)
        }
    }

    fn arguments(&mut self) -> Result<Option<HashMap<String, Value>>, ErrorKind> {
        // "(" (Name : Value)+ ")"

        if let Some(&TokenKind::LeftParan) = self.peek_token() {
            self.next_token(); // consume left parent

            let mut arguments = HashMap::new();
            loop {
                match self.argument() {
                    Ok(Some((k, v))) => {
                        arguments.insert(k, v);
                    }
                    Ok(None) => break,
                    Err(err) => return Err(err),
                }
            }

            if arguments.len() == 0 {
                return Err(ErrorKind::InvalidQuery);
            }

            self.expect(TokenKind::RightParan)?;

            Ok(Some(arguments))
        } else {
            Ok(None)
        }
    }

    fn argument(&mut self) -> Result<Option<(String, Value)>, ErrorKind> {
        if let Some(&TokenKind::Name(name)) = self.peek_token() {
            self.next_token(); // consume name

            self.expect(TokenKind::Colon)?;

            let value = self.value(false)?;

            Ok(Some((name.to_string(), value)))
        } else {
            Ok(None)
        }
    }

    fn value(&mut self, const_only: bool) -> Result<Value, ErrorKind> {
        match self.peek_token() {
            // is variable
            Some(&TokenKind::DollarSign) => {
                if const_only {
                    return Err(ErrorKind::UnexpectedToken);
                }

                self.next_token(); // consume dollar sign

                let name = match self.next_token() {
                    Some(TokenKind::Name(s)) => Ok(s.to_string()),
                    Some(TokenKind::Boolean(true)) => Ok("true".to_string()),
                    Some(TokenKind::Boolean(false)) => Ok("false".to_string()),
                    Some(TokenKind::Null) => Ok("null".to_string()),
                    Some(_) => Err(ErrorKind::UnexpectedToken),
                    None => Err(ErrorKind::ExpectedToken),
                }?;

                // default value
                if let Some(&TokenKind::EqualSign) = self.peek_token() {
                    self.next_token(); // consume equal sign

                    Ok(Value::Variable(name, Some(Box::new(self.value(true)?))))
                } else {
                    Ok(Value::Variable(name, None))
                }
            }
            // is list
            Some(&TokenKind::LeftBracket) => {
                self.next_token(); // consume left bracket

                let mut list = Vec::new();

                // TODO: vulnerable to stackoverflow attack?
                loop {
                    if let Some(&TokenKind::RightBracket) = self.peek_token() {
                        self.next_token(); // consume right bracket
                        break;
                    } else {
                        list.push(self.value(true)?);
                    }
                }

                Ok(Value::List(list))
            }
            // is object
            Some(&TokenKind::LeftBrace) => {
                self.next_token(); // consume left brace

                let mut obj = HashMap::new();

                loop {
                    match self.next_token() {
                        Some(TokenKind::RightBrace) => break,
                        Some(TokenKind::Name(name)) => {
                            self.expect(TokenKind::Colon)?;

                            let value = self.value(true)?;
                            obj.insert(name.to_string(), value);
                        }
                        Some(_) => return Err(ErrorKind::UnexpectedToken),
                        None => return Err(ErrorKind::InvalidQuery),
                    }
                }

                Ok(Value::Object(obj))
            }
            _ => {
                match self.next_token() {
                    Some(TokenKind::String(s)) => Ok(Value::String(s)),
                    Some(TokenKind::Int(i)) => Ok(Value::Int(i)),
                    Some(TokenKind::Float(f)) => Ok(Value::Float(f)),
                    Some(TokenKind::Boolean(b)) => Ok(Value::Boolean(b)),
                    Some(TokenKind::Null) => Ok(Value::Null),
                    Some(TokenKind::Name(s)) => Ok(Value::Enum(s.to_string())),
                    Some(_) => Err(ErrorKind::UnexpectedToken),
                    None => Err(ErrorKind::ExpectedToken),
                }
            }
        }
    }

    fn expect(&mut self, token: TokenKind) -> Result<(), ErrorKind> {
        match self.next_token() {
            Some(t) => {
                if t == token {
                    Ok(())
                } else {
                    Err(ErrorKind::UnexpectedToken)
                }
            }
            None => Err(ErrorKind::UnexpectedEnd),
        }
    }
}

#[cfg(test)]
mod test {
    use parser::parser::*;

    fn new_field(name: &str) -> Field {
        Field {
            alias: None,
            name: name.to_string(),
            arguments: None,
            selection_set: None,
        }
    }

    fn arg<V>(name: &str, value: V) -> HashMap<String, Value>
        where V: Into<Value>
    {
        let mut map = HashMap::new();
        map.insert(name.to_string(), value.into());
        map
    }

    #[test]
    fn ignore_unicode_bom() {
        assert_eq!(parse("\u{FEFF}{}"), Ok(SelectionSet { fields: vec![] }));
    }

    #[test]
    fn selection_set() {
        assert_eq!(parse("{"), Err(ParserError(ErrorKind::UnexpectedToken, 1)));
        assert_eq!(parse("{}"), Ok(SelectionSet { fields: vec![] }));
        assert_eq!(parse("query {}"), Ok(SelectionSet { fields: vec![] }));
        assert_eq!(parse("mutation {}"), Ok(SelectionSet { fields: vec![] }));
        assert_eq!(parse("{id name}"),
                   Ok(SelectionSet { fields: vec![new_field("id"), new_field("name")] }));
    }

    #[test]
    fn field_alias() {
        let expected = SelectionSet {
            fields: vec![Field {
                             name: "name".to_string(),
                             alias: Some("firstname".to_string()),
                             arguments: None,
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{firstname: name}"), Ok(expected));
    }

    #[test]
    fn nested_selection_set() {
        let expected = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: None,
                             selection_set: Some(SelectionSet { fields: vec![new_field("name")] }),
                         }],
        };
        assert_eq!(parse("{user { name }}"), Ok(expected));
    }

    #[test]
    fn argument_int() {
        let expected = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", 42)),
                             selection_set: Some(SelectionSet { fields: vec![new_field("name")] }),
                         }],
        };
        assert_eq!(parse("{user (id: 42) { name }}"), Ok(expected));
    }

    #[test]
    fn argument_bool() {
        let expected1 = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", true)),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: true)}"), Ok(expected1));
        let expected2 = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", false)),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: false)}"), Ok(expected2));
    }

    #[test]
    fn argument_null() {
        let expected = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", Value::Null)),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: null)}"), Ok(expected));
    }

    #[test]
    fn argument_enum() {
        let expected = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", Value::Enum("whatever".to_string()))),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: whatever)}"), Ok(expected));
    }

    #[test]
    fn argument_variable() {
        let expected1 = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id",
                                                 Value::Variable("variable".to_string(), None))),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: $variable)}"), Ok(expected1));
        let expected2 = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", Value::Variable("true".to_string(), None))),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: $true)}"), Ok(expected2));
        let expected3 = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", Value::Variable("false".to_string(), None))),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: $false)}"), Ok(expected3));
        let expected4 = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(arg("id", Value::Variable("null".to_string(), None))),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: $null)}"), Ok(expected4));
    }

    #[test]
    fn argument_variable_default_value() {
        let expected = SelectionSet {
            fields: vec![Field {
                             name: "user".to_string(),
                             alias: None,
                             arguments: Some(
                arg("id",
                    Value::Variable("foo".to_string(),
                        Some(Box::new(Value::String("bar".to_string())))))),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{user (id: $foo = \"bar\")}"), Ok(expected));
        assert_eq!(parse("{user (id: $foo = $bar)}"),
                   Err(ParserError(ErrorKind::UnexpectedToken, 18)));
    }

    #[test]
    fn argument_list() {
        let expected = SelectionSet {
            fields: vec![Field {
                             name: "update".to_string(),
                             alias: None,
                             arguments: Some(arg("data",
                                                 Value::List(vec![
                                Value::Int(42),
                                Value::String("foobar".to_string()),
                                Value::Boolean(true)]))),
                             selection_set: None,
                         }],
        };
        assert_eq!(parse("{update (data: [ 42, \"foobar\" true ] )}"),
                   Ok(expected));
        assert_eq!(parse("{update (data: [ $var] )}"),
                   Err(ParserError(ErrorKind::UnexpectedToken, 17)));
    }

    #[test]
    fn argument_object() {
        let mut expected = SelectionSet {
            fields: vec![Field {
                             name: "update".to_string(),
                             alias: None,
                             arguments: Some(arg("data", Value::Object(HashMap::new()))),
                             selection_set: None,
                         }],
        };

        assert_eq!(parse("{update (data: { } )}"), Ok(expected));

        let mut obj = HashMap::new();
        obj.insert("s".into(), "foobar".into());
        obj.insert("i".into(), 42.into());
        expected = SelectionSet {
            fields: vec![Field {
                             name: "update".to_string(),
                             alias: None,
                             arguments: Some(arg("data", Value::Object(obj))),
                             selection_set: None,
                         }],
        };

        assert_eq!(parse("{update (data: { s: \"foobar\", i: 42 } )}"),
                   Ok(expected));

        assert_eq!(parse("{update (data: { v: $var } )}"),
                   Err(ParserError(ErrorKind::UnexpectedToken, 20)));
        assert_eq!(parse("{update (data: { )}"),
                   Err(ParserError(ErrorKind::UnexpectedToken, 17)));
        assert_eq!(parse("{update (data: {}"),
                   Err(ParserError(ErrorKind::UnexpectedToken, 17)));
    }
}
