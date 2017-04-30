use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use serde_json::Value as JsonValue;
use serde_json::map::Map;

use super::lexer::{Lexer, TokenKind, LexerError};
use super::value::Value;

#[derive(PartialEq, Debug)]
pub struct Field {
    pub alias: Option<String>,
    pub name: String,
    pub arguments: Option<HashMap<String, Value>>,
    pub selection_set: Option<SelectionSet>,
}

#[derive(PartialEq, Debug)]
pub struct SelectionSet {
    pub fields: Rc<RefCell<Vec<Field>>>,
}

struct Parser<'a> {
    vars: &'a Map<String, JsonValue>,
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
    VariableMissing,
    NonNullable,
    UnexpectedVariable,
    InvalidArgumentType,
}

#[derive(Debug)]
pub enum VariableType {
    Named(String, bool),
    List(Box<VariableType>, bool),
}

impl VariableType {
    fn is_nullable(&self) -> bool {
        match self {
            &VariableType::Named(_, nullable) => nullable,
            &VariableType::List(_, nullable) => nullable,
        }
    }
}

impl ParserError {
    pub fn as_str(&self) -> String {
        match self.0 {
            ErrorKind::ExpectedToken => format!("expected token at {}", self.1),
            ErrorKind::UnexpectedToken => format!("unexpected token at {}", self.1),
            ErrorKind::LexerError(_) => format!("syntax error at {}", self.1),
            ErrorKind::UnexpectedEnd => format!("unexpected end at {}", self.1),
            ErrorKind::InvalidQuery => format!("invalid query at {}", self.1),
            ErrorKind::VariableMissing => format!("variable missing at {}", self.1),
            ErrorKind::NonNullable => format!("variable is not nullable at {}", self.1),
            ErrorKind::UnexpectedVariable => format!("unexpected variable at {}", self.1),
            ErrorKind::InvalidArgumentType => format!("invalid argument type at {}", self.1),
        }
    }
}

impl From<LexerError> for ErrorKind {
    fn from(err: LexerError) -> ErrorKind {
        ErrorKind::LexerError(err)
    }
}

pub fn parse(src: &str, vars: &Map<String, JsonValue>) -> Result<SelectionSet, ParserError> {
    let mut parser = Parser::new(src, vars);
    parser.document()
}

impl<'a> Parser<'a> {
    fn new(src: &'a str, vars: &'a Map<String, JsonValue>) -> Parser<'a> {
        let lexer = Lexer::new(src);
        Parser {
            vars: vars,
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

        // OperationType
        if let Some(&TokenKind::Name(operation)) = self.peek_token() {
            self.next_token(); // consume name

            if operation != "query" && operation != "mutation" {
                return Err(ErrorKind::InvalidQuery);
            }
        }

        let mut variables = None;

        // VariableDefinitions
        if let Some(v) = self.variable_definitions()? {
            let mut vars = HashMap::new();

            for (k, (t, d)) in v {
                let val = self.vars
                    .get(&k)
                    .map(|v| value_from_json_named(&t, v))
                    .or_else(|| d.map(|v| Ok(v)));

                if let Some(val) = val {
                    let val = val?;

                    vars.insert(k, val);
                }
            }

            variables = Some(vars)
        }

        match self.selection_set(variables.as_ref()) {
            Ok(Some(ss)) => Ok(ss),
            Ok(None) => Err(ErrorKind::InvalidQuery),
            Err(err) => Err(err),
        }
    }

    // TODO: tests for variable_definitions
    fn variable_definitions
        (&mut self)
         -> Result<Option<HashMap<String, (VariableType, Option<Value>)>>, ErrorKind> {
        if let Some(&TokenKind::LeftParan) = self.peek_token() {
            self.next_token(); // consume left parent

            let mut variables = HashMap::new();
            loop {
                match self.peek_token() {
                    // variable
                    Some(&TokenKind::DollarSign) => {
                        self.next_token(); // consume dollar sign

                        let name = match self.next_token() {
                            Some(TokenKind::Name(s)) => Ok(s.to_string()),
                            Some(TokenKind::Boolean(true)) => Ok("true".to_string()),
                            Some(TokenKind::Boolean(false)) => Ok("false".to_string()),
                            Some(TokenKind::Null) => Ok("null".to_string()),
                            Some(_) => Err(ErrorKind::UnexpectedToken),
                            None => Err(ErrorKind::ExpectedToken),
                        }?;

                        self.expect(TokenKind::Colon)?;

                        let input_type = self.input_type()?;
                        let mut default = None;

                        // default value
                        if let Some(&TokenKind::EqualSign) = self.peek_token() {
                            self.next_token(); // consume equal sign

                            default = Some(self.value(None, true)?);
                        }

                        variables.insert(name, (input_type, default));
                    }
                    Some(_) | None => break,
                }
            }

            if variables.len() == 0 {
                return Err(ErrorKind::InvalidQuery);
            }

            self.expect(TokenKind::RightParan)?;

            Ok(Some(variables))
        } else {
            Ok(None)
        }
    }

    fn selection_set(&mut self,
                     vars: Option<&HashMap<String, Value>>)
                     -> Result<Option<SelectionSet>, ErrorKind> {
        // TODO: "{" (Field | FragmentSpread | InlineFragment)+ "}"

        if let Some(&TokenKind::LeftBrace) = self.peek_token() {
            self.next_token(); // consume left brace

            let mut fields = vec![];
            loop {
                match self.field(vars) {
                    Ok(Some(field)) => fields.push(field),
                    Ok(None) => break,
                    Err(err) => return Err(err),
                }
            }

            self.expect(TokenKind::RightBrace)?;

            Ok(Some(SelectionSet { fields: Rc::new(RefCell::new(fields)) }))
        } else {
            Ok(None)
        }
    }

    fn field(&mut self, vars: Option<&HashMap<String, Value>>) -> Result<Option<Field>, ErrorKind> {
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

            let arguments = self.arguments(vars)?;
            let selection_set = self.selection_set(vars)?;

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

    fn arguments(&mut self,
                 vars: Option<&HashMap<String, Value>>)
                 -> Result<Option<HashMap<String, Value>>, ErrorKind> {
        // "(" (Name : Value)+ ")"

        if let Some(&TokenKind::LeftParan) = self.peek_token() {
            self.next_token(); // consume left parent

            let mut arguments = HashMap::new();
            loop {
                match self.argument(vars) {
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

    fn argument(&mut self,
                vars: Option<&HashMap<String, Value>>)
                -> Result<Option<(String, Value)>, ErrorKind> {
        if let Some(&TokenKind::Name(name)) = self.peek_token() {
            self.next_token(); // consume name

            self.expect(TokenKind::Colon)?;

            let value = self.value(vars, false)?;
            Ok(Some((name.to_string(), value)))
        } else {
            Ok(None)
        }
    }

    fn value(&mut self,
             vars: Option<&HashMap<String, Value>>,
             const_only: bool)
             -> Result<Value, ErrorKind> {
        match self.peek_token() {
            // is variable
            Some(&TokenKind::DollarSign) => {
                if const_only {
                    return Err(ErrorKind::UnexpectedToken);
                }

                self.next_token(); // consume dollar sign

                let name = match self.next_token() {
                    Some(TokenKind::Name(s)) => Ok(s),
                    Some(TokenKind::Boolean(true)) => Ok("true"),
                    Some(TokenKind::Boolean(false)) => Ok("false"),
                    Some(TokenKind::Null) => Ok("null"),
                    Some(_) => Err(ErrorKind::UnexpectedToken),
                    None => Err(ErrorKind::ExpectedToken),
                }?;

                // default value
                let default = if let Some(&TokenKind::EqualSign) = self.peek_token() {
                    self.next_token(); // consume equal sign

                    Some(self.value(None, true)?)
                } else {
                    None
                };

                // coerce variable
                let coerce = vars.and_then(|m| m.get(name).map(|r| (*r).clone()))
                    .or(default);
                match coerce {
                    Some(val) => Ok(val),
                    None => return Err(ErrorKind::UnexpectedVariable),
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
                        list.push(self.value(None, true)?);
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

                            let value = self.value(None, true)?;
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

    // TODO: tests for input_type
    fn input_type(&mut self) -> Result<VariableType, ErrorKind> {
        match self.next_token() {
            Some(TokenKind::Name(s)) => {
                if let Some(&TokenKind::Bang) = self.peek_token() {
                    self.next_token(); // consume
                    Ok(VariableType::Named(s.to_string(), false))
                } else {
                    Ok(VariableType::Named(s.to_string(), true))
                }
            }
            Some(TokenKind::LeftBracket) => {
                // TODO: prevent stackoverflow attacks
                let inner = Box::new(self.input_type()?);

                self.expect(TokenKind::RightBracket)?;

                if let Some(&TokenKind::Bang) = self.peek_token() {
                    self.next_token(); // consume
                    Ok(VariableType::List(inner, false))
                } else {
                    Ok(VariableType::List(inner, true))
                }
            }
            Some(_) | None => Err(ErrorKind::UnexpectedToken),
            // None => Err(ErrorKind::ExpectedToken),
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

fn value_from_json_named(t: &VariableType, v: &JsonValue) -> Result<Value, ErrorKind> {
    if let &JsonValue::Null = v {
        if t.is_nullable() {
            return Ok(Value::Null)
        } else {
            return Err(ErrorKind::NonNullable)
        }
    }

    match t {
        &VariableType::Named(ref name, _) => {
            match name.as_str() {
                "Int" => {
                    if let &JsonValue::Number(ref n) = v {
                        if n.is_i64() {
                            Ok(Value::Int(n.as_i64().unwrap() as i32))
                        } else if n.is_u64() {
                            Ok(Value::Int(n.as_u64().unwrap() as i32))
                        } else {
                            Err(ErrorKind::InvalidArgumentType)
                        }
                    } else {
                        Err(ErrorKind::InvalidArgumentType)
                    }
                }
                "Float" => {
                    if let &JsonValue::Number(ref n) = v {
                        if n.is_f64() {
                            Ok(Value::Float(n.as_f64().unwrap() as f32))
                        } else {
                            Err(ErrorKind::InvalidArgumentType)
                        }
                    } else {
                        Err(ErrorKind::InvalidArgumentType)
                    }
                },
                "String" => {
                    if let &JsonValue::String(ref s) = v {
                        Ok(Value::String(s.clone()))
                    } else {
                        Err(ErrorKind::InvalidArgumentType)
                    }
                }
                _ => panic!("variable type {} unimplemented", name),
            }
        }
        &VariableType::List(ref t, _) => {
            if let &JsonValue::Array(ref vec) = v {
                let mut result = Vec::with_capacity(vec.len());
                for v in vec.iter() {
                    result.push(value_from_json_named(t, v)?)
                }
                Ok(Value::List(result))
            } else {
                Err(ErrorKind::InvalidArgumentType)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::cell::RefCell;
    use parser::parser::*;
    use parser::parser::parse as parse_original;
    use serde_json::map::Map;

    fn parse(src: &str) -> Result<SelectionSet, ParserError> {
        let vars = Map::new();
        parse_original(src, &vars)
    }

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

    fn fields(vec: Vec<Field>) -> Rc<RefCell<Vec<Field>>> {
        Rc::new(RefCell::new(vec))
    }

    #[test]
    fn from_gql() {
        use parser::value::FromGql;

        let val = &Value::Int(42);
        assert_eq!(i32::from_gql(val), Ok(42));
    }

    #[test]
    fn ignore_unicode_bom() {
        assert_eq!(parse("\u{FEFF}{}"),
                   Ok(SelectionSet { fields: fields(vec![]) }));
    }

    #[test]
    fn selection_set() {
        assert_eq!(parse("{"), Err(ParserError(ErrorKind::UnexpectedToken, 1)));
        assert_eq!(parse("{}"), Ok(SelectionSet { fields: fields(vec![]) }));
        assert_eq!(parse("query {}"),
                   Ok(SelectionSet { fields: fields(vec![]) }));
        assert_eq!(parse("mutation {}"),
                   Ok(SelectionSet { fields: fields(vec![]) }));
        assert_eq!(parse("{id name}"),
                   Ok(SelectionSet { fields: fields(vec![new_field("id"), new_field("name")]) }));
    }

    #[test]
    fn field_alias() {
        let expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "name".to_string(),
                                    alias: Some("firstname".to_string()),
                                    arguments: None,
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("{firstname: name}"), Ok(expected));
    }

    #[test]
    fn nested_selection_set() {
        let expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: None,
                                    selection_set: Some(SelectionSet {
                                                            fields: fields(vec![new_field("name")]),
                                                        }),
                                }]),
        };
        assert_eq!(parse("{user { name }}"), Ok(expected));
    }

    #[test]
    fn argument_int() {
        let expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", 42)),
                                    selection_set: Some(SelectionSet {
                                                            fields: fields(vec![new_field("name")]),
                                                        }),
                                }]),
        };
        assert_eq!(parse("{user (id: 42) { name }}"), Ok(expected));
    }

    #[test]
    fn argument_bool() {
        let expected1 = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", true)),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("{user (id: true)}"), Ok(expected1));
        let expected2 = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", false)),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("{user (id: false)}"), Ok(expected2));
    }

    #[test]
    fn argument_null() {
        let expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", Value::Null)),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("{user (id: null)}"), Ok(expected));
    }

    #[test]
    fn argument_enum() {
        let expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", Value::Enum("whatever".to_string()))),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("{user (id: whatever)}"), Ok(expected));
    }

    #[test]
    fn argument_variable() {
        let expected1 = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", Value::Int(1))),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("query ($var: Int = 1) {user (id: $var)}"),
                   Ok(expected1));
        let expected2 = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", Value::Int(2))),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("query ($true: Int = 2) {user (id: $true)}"),
                   Ok(expected2));
        let expected3 = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", Value::Int(3))),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("query ($false: Int = 3) {user (id: $false)}"),
                   Ok(expected3));
        let expected4 = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", Value::Int(4))),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("query ($null: Int = 4) {user (id: $null)}"),
                   Ok(expected4));
    }

    #[test]
    fn argument_variable_default_value() {
        let expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "user".to_string(),
                                    alias: None,
                                    arguments: Some(arg("id", Value::String("bar".to_string()))),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("query ($foo: String) {user (id: $foo = \"bar\")}"),
                   Ok(expected));
        assert_eq!(parse("{user (id: $foo = $bar)}"),
                   Err(ParserError(ErrorKind::UnexpectedToken, 18)));
    }

    #[test]
    fn argument_list() {
        let expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "update".to_string(),
                                    alias: None,
                                    arguments: Some(arg("data",
                                                        Value::List(vec![
                                Value::Int(42),
                                Value::String("foobar".to_string()),
                                Value::Boolean(true)]))),
                                    selection_set: None,
                                }]),
        };
        assert_eq!(parse("{update (data: [ 42, \"foobar\" true ] )}"),
                   Ok(expected));
        assert_eq!(parse("{update (data: [ $var] )}"),
                   Err(ParserError(ErrorKind::UnexpectedToken, 17)));
    }

    #[test]
    fn argument_object() {
        let mut expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "update".to_string(),
                                    alias: None,
                                    arguments: Some(arg("data", Value::Object(HashMap::new()))),
                                    selection_set: None,
                                }]),
        };

        assert_eq!(parse("{update (data: { } )}"), Ok(expected));

        let mut obj = HashMap::new();
        obj.insert("s".into(), "foobar".into());
        obj.insert("i".into(), 42.into());
        expected = SelectionSet {
            fields: fields(vec![Field {
                                    name: "update".to_string(),
                                    alias: None,
                                    arguments: Some(arg("data", Value::Object(obj))),
                                    selection_set: None,
                                }]),
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
