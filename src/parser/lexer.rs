use std::str::CharIndices;
use std::iter::{Iterator, Peekable};

#[derive(Debug)]
pub struct Lexer<'a> {
    iter: Peekable<CharIndices<'a>>,
    src: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    // single-charachter tokens
    LeftBrace,
    RightBrace,
    LeftParan,
    RightParan,
    Colon,

    // two-character tokens

    // literals
    Name(&'a str),
    String(String),
    Int(i32),
    Float(f32),
    Boolean(bool),

    // keywords
    EOF,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidEscapeSequence,
    InvalidNumber,
}

impl<'a> Lexer<'a> {
    #[doc(hidden)]
    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            iter: src.char_indices().peekable(),
            src: src,
        }
    }

    pub fn scan(&mut self) -> Result<Token<'a>, LexerError> {
        if let Some(&(_, c)) = self.iter.peek() {
            match c {
                '"' => return self.scan_string(),
                '-' | '0'...'9' => return self.scan_number(),
                '_' | 'a'...'z' | 'A'...'Z' => return self.scan_identifier(),
                '{' => {
                    self.iter.next();
                    Ok(Token::LeftBrace)
                }
                '}' => {
                    self.iter.next();
                    Ok(Token::RightBrace)
                }
                '(' => {
                    self.iter.next();
                    Ok(Token::LeftParan)
                }
                ')' => {
                    self.iter.next();
                    Ok(Token::RightParan)
                }
                ':' => {
                    self.iter.next();
                    Ok(Token::Colon)
                }
                '\n' | '\r' | '\t' | ' ' => {
                    self.iter.next();
                    self.scan()
                }
                _ => Err(LexerError::UnexpectedCharacter(c)), // TODO: possible stack trace error?
            }
        } else {
            Ok(Token::EOF)
        }

    }

    fn scan_identifier(&mut self) -> Result<Token<'a>, LexerError> {
        let mut from = 0;
        let mut to = 0;

        if let Some(&(i, _)) = self.iter.peek() {
            from = i;
            to = i
        }

        // consume digits
        while let Some(&(i, c)) = self.iter.peek() {
            match c {
                '_' | 'a'...'z' | 'A'...'Z' | '0'...'9' => {
                    self.iter.next();
                    to = i + 1;
                }
                _ => break,
            }
        }

        let string = &self.src[from..to];
        match string {
            "true" => Ok(Token::Boolean(true)),
            "false" => Ok(Token::Boolean(false)),
            _ => Ok(Token::Name(string)),
        }
    }

    fn scan_escaped_character(&mut self) -> Option<char> {
        use core::char::from_u32;

        match self.iter.next() {
            Some((_, '"')) => Some('"'),
            Some((_, '/')) => Some('/'),
            Some((_, '\\')) => Some('\\'),
            Some((_, 'b')) => Some('\u{0008}'),
            Some((_, 'f')) => Some('\u{000c}'),
            Some((_, 'n')) => Some('\n'),
            Some((_, 'r')) => Some('\r'),
            Some((_, 't')) => Some('\t'),
            Some((from, 'u')) => {
                for _ in 0..4 {
                    if let Some((_, c)) = self.iter.next() {
                        match c {
                            '0'...'9' | 'A'...'F' | 'a'...'f' => continue,
                            _ => return None,
                        }
                    }
                }

                match u32::from_str_radix(&self.src[from + 1..from + 5], 16) {
                    Ok(code) => from_u32(code),
                    Err(_) => None,
                }
            }
            _ => None,
        }
    }

    fn scan_string(&mut self) -> Result<Token<'a>, LexerError> {
        let mut from = None;

        // advance iterator to skip starting quote
        self.iter.next();

        let mut string = String::new();

        while let Some((i, c)) = self.iter.next() {
            if from == None {
                from = Some(i);
            }

            match c {
                '"' => {
                    if let Some(f) = from {
                        string.push_str(&self.src[f..i]);
                    }

                    return Ok(Token::String(string));
                }
                '\\' => {
                    if let Some(f) = from {
                        string.push_str(&self.src[f..i]);
                    }

                    from = None;
                    match self.scan_escaped_character() {
                        Some(c) => string.push(c),
                        None => return Err(LexerError::InvalidEscapeSequence),
                    }
                }
                // allowed source characters are /[\u0009\u000A\u000D\u0020-\uFFFF]/
                // in this case except LF (\u000A) and CR (\u000D)
                '\u{0000}'...'\u{008}' |
                '\u{000A}'...'\u{001F}' => return Err(LexerError::UnexpectedCharacter(c)),
                _ => continue,
            }
        }

        Err(LexerError::UnterminatedString)
    }

    fn scan_number(&mut self) -> Result<Token<'a>, LexerError> {
        let mut from = 0;
        let mut to = 0;

        if let Some(&(i, _)) = self.iter.peek() {
            from = i;
            to = i + 1;
        }

        // if starting with a -, just consume it
        if let Some(&(_, '-')) = self.iter.peek() {
            self.iter.next();
        }

        // if starting with with a zero ...
        if let Some(&(i, '0')) = self.iter.peek() {
            self.iter.next(); // consume
            to = i + 1;
        } else {
            // consume digits
            while let Some(&(i, '0'...'9')) = self.iter.peek() {
                self.iter.next();
                to = i + 1;
            }
        }

        let mut is_float = false;

        // has fraction?
        if let Some(&(_, '.')) = self.iter.peek() {
            self.iter.next(); // consume

            // consume digits
            while let Some(&(i, '0'...'9')) = self.iter.peek() {
                self.iter.next();
                to = i + 1;
                is_float = true;
            }
        }

        // has exponent?
        match self.iter.peek() {
            Some(&(_, 'e')) | Some(&(_, 'E')) => {
                self.iter.next(); // consume

                // allow + and - signs
                if let Some(&(_, c)) = self.iter.peek() {
                    if c == '+' || c == '-' {
                        self.iter.next();
                    }
                }

                // consume digits
                while let Some(&(i, '0'...'9')) = self.iter.peek() {
                    self.iter.next();
                    to = i + 1;
                    is_float = true;
                }
            }
            Some(&(_, _)) | None => {}
        }

        // convert to int or float
        let string = &self.src[from..to];
        if is_float {
            match string.parse() {
                Ok(float) => return Ok(Token::Float(float)),
                Err(_) => return Err(LexerError::InvalidNumber),
            }
        } else {
            match string.parse() {
                Ok(int) => return Ok(Token::Int(int)),
                Err(_) => return Err(LexerError::InvalidNumber),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use parser::lexer::*;
    use std;

    #[test]
    fn scan() {
        let mut lexer = Lexer::new("{}");
        assert_eq!(lexer.scan(), Ok(Token::LeftBrace));
        assert_eq!(lexer.scan(), Ok(Token::RightBrace));
        assert_eq!(lexer.scan(), Ok(Token::EOF));
    }

    #[test]
    fn value_int() {
        assert_eq!(Lexer::new("0").scan(), Ok(Token::Int(0)));
        assert_eq!(Lexer::new("-0").scan(), Ok(Token::Int(0)));
        assert_eq!(Lexer::new("42").scan(), Ok(Token::Int(42)));
        assert_eq!(Lexer::new("-42").scan(), Ok(Token::Int(-42)));

        // skipping wrong number parts
        assert_eq!(Lexer::new("042").scan(), Ok(Token::Int(0)));
        // assert_eq!(Lexer::new("042").scan(), unexpected(b"042"));
        assert_eq!(Lexer::new("-042").scan(), Ok(Token::Int(0)));
        // assert_eq!(Lexer::new("-042").scan(), unexpected(b"042"));

        // max values
        assert_eq!(Lexer::new("2147483647").scan(), Ok(Token::Int(2147483647)));
        assert_eq!(Lexer::new("-2147483648").scan(),
                   Ok(Token::Int(-2147483648)));

        // test overflow
        assert_eq!(Lexer::new("2147483648").scan(),
                   Err(LexerError::InvalidNumber));
        assert_eq!(Lexer::new("-2147483649").scan(),
                   Err(LexerError::InvalidNumber));
    }

    #[test]
    fn value_float() {
        assert_eq!(Lexer::new("1.0").scan(), Ok(Token::Float(1.0)));
        assert_eq!(Lexer::new("0.0").scan(), Ok(Token::Float(0.0)));
        assert_eq!(Lexer::new("0.04").scan(), Ok(Token::Float(0.04)));

        assert_eq!(Lexer::new("4.2e5").scan(), Ok(Token::Float(420000.0)));
        assert_eq!(Lexer::new("4.2E5").scan(), Ok(Token::Float(420000.0)));
        assert_eq!(Lexer::new("4.2e+5").scan(), Ok(Token::Float(420000.0)));
        assert_eq!(Lexer::new("4.2E+5").scan(), Ok(Token::Float(420000.0)));
        assert_eq!(Lexer::new("4.2e-5").scan(), Ok(Token::Float(0.000042)));
        assert_eq!(Lexer::new("4.2E-5").scan(), Ok(Token::Float(0.000042)));
        assert_eq!(Lexer::new("42e+5").scan(), Ok(Token::Float(4200000.0)));
        assert_eq!(Lexer::new("42E+5").scan(), Ok(Token::Float(4200000.0)));
        assert_eq!(Lexer::new("42e-5").scan(), Ok(Token::Float(0.00042)));
        assert_eq!(Lexer::new("42E-5").scan(), Ok(Token::Float(0.00042)));

        // skipping wrong number parts
        assert_eq!(Lexer::new("42.").scan(), Ok(Token::Int(42)));
        assert_eq!(Lexer::new("42.0e").scan(), Ok(Token::Float(42.0)));
        assert_eq!(Lexer::new("42.0E").scan(), Ok(Token::Float(42.0)));
        assert_eq!(Lexer::new("42e+").scan(), Ok(Token::Int(42)));
        assert_eq!(Lexer::new("42E-").scan(), Ok(Token::Int(42)));

        // max values
        assert_eq!(Lexer::new("3.40282347e+38").scan(),
                   Ok(Token::Float(std::f32::MAX)));
        assert_eq!(Lexer::new("-3.4028235e+38").scan(),
                   Ok(Token::Float(std::f32::MIN)));

        // test overflow
        assert_eq!(Lexer::new("340282350000000000000000000000000000001").scan(),
                   Err(LexerError::InvalidNumber));
        assert_eq!(Lexer::new("-340282350000000000000000000000000000001").scan(),
                   Err(LexerError::InvalidNumber));
    }

    #[test]
    fn value_boolean() {
        assert_eq!(Lexer::new("true").scan(), Ok(Token::Boolean(true)));
        assert_eq!(Lexer::new("false").scan(), Ok(Token::Boolean(false)));
    }

    #[test]
    fn value_string() {
        assert_eq!(Lexer::new(r#"""#).scan(),
                   Err(LexerError::UnterminatedString));
        assert_eq!(Lexer::new(r#""""#).scan(),
                   Ok(Token::String("".to_string())));
        assert_eq!(Lexer::new(r#""foobar""#).scan(),
                   Ok(Token::String("foobar".to_string())));
        assert_eq!(Lexer::new(r#""foo\"bar""#).scan(),
                   Ok(Token::String(r#"foo"bar"#.to_string())));
        assert_eq!(Lexer::new(r#""🦈""#).scan(),
                   Ok(Token::String(r#"🦈"#.to_string())));
        assert_eq!(Lexer::new(r#""a
            b""#)
                           .scan(),
                   Err(LexerError::UnexpectedCharacter('\n')));
        assert_eq!(Lexer::new("\"\t\"").scan(),
                   Ok(Token::String("\t".to_string())));

        assert_eq!(Lexer::new(r#""\b""#).scan(),
                   Ok(Token::String("\u{0008}".to_string())));
        assert_eq!(Lexer::new(r#""\f""#).scan(),
                   Ok(Token::String("\u{000c}".to_string())));
        assert_eq!(Lexer::new(r#""\n""#).scan(),
                   Ok(Token::String("\n".to_string())));
        assert_eq!(Lexer::new(r#""\r""#).scan(),
                   Ok(Token::String("\r".to_string())));
        assert_eq!(Lexer::new(r#""\t""#).scan(),
                   Ok(Token::String("\t".to_string())));

        assert_eq!(Lexer::new("\"\\u2699\"").scan(),
                   Ok(Token::String("⚙".to_string())));
    }
}