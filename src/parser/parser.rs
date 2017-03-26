use super::lexer::{Lexer, Token, LexerError};
use ::{Resolvable, Value as ResolveValue, ResolveError, resolve};
use futures::{future, Future, BoxFuture};

#[derive(PartialEq, Debug)]
pub enum Value {
    Variable(String, Option<Box<Value>>),
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Null,
    Enum(String),
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

struct Parser<'a> {
    lexer: Lexer<'a>,
    lexer_error: Option<LexerError>,
    peeked: Option<Token<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    ExpectedToken,
    UnexpectedToken,
    LexerError(LexerError),
    UnexpectedEnd,
    InvalidQuery,
}

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> ParserError {
        ParserError::LexerError(err)
    }
}

pub fn parse(src: &str) -> Result<SelectionSet, ParserError> {
    let mut parser = Parser::new(src);
    parser.document()
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Parser {
        let lexer = Lexer::new(src);
        Parser { lexer: lexer, lexer_error: None, peeked: None }
    }

    #[inline]
    fn next_token(&mut self) -> Option<Token<'a>> {
        match self.peeked {
            Some(_) => self.peeked.take(),
            None => {
                match self.lexer.scan() {
                    Ok(token) => Some(token),
                    Err(err) => {
                        self.lexer_error = Some(err);
                        None
                    }
                }
            },
        }
    }

    #[inline]
    fn peek_token(&mut self) -> Option<&Token<'a>> {
        if self.peeked.is_none() {
            self.peeked = match self.lexer.scan() {
                Ok(token) => Some(token),
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

      self.operation().map_err(|err| {
        if let Some(err) = self.lexer_error.take() {
            err.into()
        } else {
            err
        }
      })
    }

    fn operation(&mut self) -> Result<SelectionSet, ParserError> {
      // TODO: SelectionSet | (("query" | "mutation") [Name] [VariableDefinitions] [Directives] SelectionSet)

      if let Some(&Token::Name(operation)) = self.peek_token() {
          self.next_token(); // consume name

          if operation != "query" && operation != "mutation" {
            return Err(ParserError::InvalidQuery)
          }
      }

      match self.selection_set() {
        Ok(Some(ss)) => Ok(ss),
        Ok(None) => Err(ParserError::InvalidQuery),
        Err(err) => Err(err)
      }
    }

    fn selection_set(&mut self) -> Result<Option<SelectionSet>, ParserError> {
        // TODO: "{" (Field | FragmentSpread | InlineFragment)+ "}"

        if let Some(&Token::LeftBrace) = self.peek_token() {
            self.next_token(); // consume left brace

            let mut fields = vec![];
            loop {
              match self.field() {
                  Ok(Some(field)) => fields.push(field),
                  Ok(None) => break,
                  Err(err) => return Err(err)
              }
            }

            self.expect(Token::RightBrace)?;

            Ok(Some(SelectionSet{
              fields: fields,
            }))
        } else {
            Ok(None)
        }
    }

    fn field(&mut self) -> Result<Option<Field>, ParserError> {
        // TODO: [Alias] Name [Arguments] [Directives] [SelectionSet]

        if let Some(&Token::Name(n)) = self.peek_token() {
            self.next_token(); // consume name

            let mut name = n.to_string();
            let mut alias = None;

            if let Some(&Token::Colon) = self.peek_token() {
                self.next_token(); // consume colon

                match self.next_token() {
                    Some(Token::Name(a)) => {
                        alias = Some(name);
                        name = a.to_string()
                    },
                    Some(_) => return Err(ParserError::UnexpectedToken),
                    None => return Err(ParserError::InvalidQuery)
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

    fn arguments(&mut self) -> Result<Option<Vec<(String, Value)>>, ParserError> {
        // "(" (Name : Value)+ ")"

        if let Some(&Token::LeftParan) = self.peek_token() {
            self.next_token(); // consume left parent

            let mut arguments = vec![];
            loop {
              match self.argument() {
                  Ok(Some(argument)) => arguments.push(argument),
                  Ok(None) => break,
                  Err(err) => return Err(err)
              }
            }

            if arguments.len() == 0 {
                return Err(ParserError::InvalidQuery)
            }

            self.expect(Token::RightParan)?;

            Ok(Some(arguments))
        } else {
            Ok(None)
        }
    }

    fn argument(&mut self) -> Result<Option<(String, Value)>, ParserError> {
        if let Some(&Token::Name(name)) = self.peek_token() {
            self.next_token(); // consume name

            self.expect(Token::Colon)?;

            let value = self.value(false)?;

            Ok(Some((name.to_string(), value)))
        } else {
            Ok(None)
        }
    }

    fn value(&mut self, const_only: bool) -> Result<Value, ParserError> {
        // is variable
        if let Some(&Token::DollarSign) = self.peek_token() {
            if const_only {
                return Err(ParserError::UnexpectedToken)
            }

            self.next_token(); // consume dollar sign

            let name = match self.next_token() {
               Some(Token::Name(s)) => s.to_string(),
               Some(Token::Boolean(true)) => "true".to_string(),
               Some(Token::Boolean(false)) => "false".to_string(),
               Some(Token::Null) => "null".to_string(),
               Some(_) | None => String::new(),
            };

            if name.len() == 0 {
                return Err(ParserError::UnexpectedToken)
            }

            // default value
            if let Some(&Token::EqualSign) = self.peek_token() {
                self.next_token(); // consume equal sign

                Ok(Value::Variable(name, Some(Box::new(self.value(true)?))))
            } else {
                Ok(Value::Variable(name, None))
            }
        } else {
            match self.next_token() {
                Some(Token::String(s)) => Ok(Value::String(s)),
                Some(Token::Int(i)) => Ok(Value::Int(i)),
                Some(Token::Float(f)) => Ok(Value::Float(f)),
                Some(Token::Boolean(b)) => Ok(Value::Boolean(b)),
                Some(Token::Null) => Ok(Value::Null),
                Some(Token::Name(s)) => Ok(Value::Enum(s.to_string())),
                Some(_) => Err(ParserError::UnexpectedToken),
                None => Err(ParserError::ExpectedToken),
            }
        }
    }

    fn expect(&mut self, token: Token) -> Result<(), ParserError> {
        match self.next_token() {
            Some(t) => {
                if t == token {
                    Ok(())
                } else {
                    println!("asdasd {:?} {:?}",t, token );
                    Err(ParserError::UnexpectedToken)
                }
            },
            None => Err(ParserError::UnexpectedEnd)
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

    #[test]
    fn selection_set() {
        assert_eq!(parse("{"), Err(ParserError::UnexpectedToken));
        assert_eq!(parse("{}"), Ok(SelectionSet{fields:vec![]}));
        assert_eq!(parse("query {}"), Ok(SelectionSet{fields:vec![]}));
        assert_eq!(parse("mutation {}"), Ok(SelectionSet{fields:vec![]}));
        assert_eq!(parse("{id name}"), Ok(SelectionSet{fields:vec![new_field("id"), new_field("name")]}));
        assert_eq!(parse("{firstname: name}"), Ok(SelectionSet{fields:vec![Field{
            name: "name".to_string(),
            alias: Some("firstname".to_string()),
            arguments: None,
            selection_set: None,
        }]}));
        assert_eq!(parse("{user { name }}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: None,
            selection_set: Some(SelectionSet{
                fields: vec![new_field("name")]
            }),
        }]}));
        assert_eq!(parse("{user (id: 42) { name }}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Int(42))]),
            selection_set: Some(SelectionSet{
                fields: vec![new_field("name")]
            }),
        }]}));
        assert_eq!(parse("{user (id: true)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Boolean(true))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: false)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Boolean(false))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: null)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Null)]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: whatever)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Enum("whatever".to_string()))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: $variable)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Variable("variable".to_string(), None))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: $true)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Variable("true".to_string(), None))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: $false)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Variable("false".to_string(), None))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: $null)}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Variable("null".to_string(), None))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: $foo = \"bar\")}"), Ok(SelectionSet{fields:vec![Field{
            name: "user".to_string(),
            alias: None,
            arguments: Some(vec![("id".to_string(), Value::Variable("foo".to_string(), Some(Box::new(Value::String("bar".to_string())))))]),
            selection_set: None,
        }]}));
        assert_eq!(parse("{user (id: $foo = $bar)}"), Err(ParserError::UnexpectedToken));
    }
}