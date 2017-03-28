use std::str::CharIndices;
use std::iter::{Iterator, Peekable};

#[derive(Debug)]
pub struct Lexer<'a> {
    iter: Peekable<CharIndices<'a>>,
    src: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct Token<'a>(pub TokenKind<'a>, pub usize);

#[derive(Debug, PartialEq)]
pub enum TokenKind<'a> {
    // single-charachter tokens
    LeftBrace,
    RightBrace,
    LeftParan,
    RightParan,
    Colon,
    DollarSign,
    EqualSign,

    // two-character tokens

    // literals
    Name(&'a str),
    String(String),
    Int(i32),
    Float(f32),
    Boolean(bool),
    Null,

    // keywords
    EOF,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LexerError(pub ErrorKind, pub usize);

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedCharacter,
    UnterminatedString,
    InvalidEscapeSequence,
    InvalidNumber,
}

impl<'a> Lexer<'a> {
    #[doc(hidden)]
    pub fn new(src: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            iter: src.char_indices().peekable(),
            src: src,
        };

        if let Some(&(_, '\u{FEFF}')) = lexer.iter.peek() {
            // skip unicode bom
            lexer.iter.next();
        }

        lexer
    }

    pub fn scan(&mut self) -> Result<Token<'a>, LexerError> {
        loop {
            if let Some(&(i, c)) = self.iter.peek() {
                match c {
                    '"' => return self.scan_string(i),
                    '-' | '0'...'9' => return self.scan_number(i),
                    '_' | 'a'...'z' | 'A'...'Z' => return self.scan_identifier(i),
                    '{' => {
                        self.iter.next();
                        return Ok(Token(TokenKind::LeftBrace, i));
                    }
                    '}' => {
                        self.iter.next();
                        return Ok(Token(TokenKind::RightBrace, i));
                    }
                    '(' => {
                        self.iter.next();
                        return Ok(Token(TokenKind::LeftParan, i));
                    }
                    ')' => {
                        self.iter.next();
                        return Ok(Token(TokenKind::RightParan, i));
                    }
                    ':' => {
                        self.iter.next();
                        return Ok(Token(TokenKind::Colon, i));
                    }
                    '=' => {
                        self.iter.next();
                        return Ok(Token(TokenKind::EqualSign, i));
                    }
                    '$' => {
                        self.iter.next();
                        return Ok(Token(TokenKind::DollarSign, i));
                    }
                    '\n' | '\r' | '\t' | ' ' => {
                        self.iter.next();
                        // continue in loop
                    }
                    _ => return Err(LexerError(ErrorKind::UnexpectedCharacter, i)),
                }
            } else {
                return Ok(Token(TokenKind::EOF, self.src.len()));
            }
        }
    }

    fn scan_identifier(&mut self, i: usize) -> Result<Token<'a>, LexerError> {
        let from = i;
        let mut to = i;

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
            "true" => Ok(Token(TokenKind::Boolean(true), from)),
            "false" => Ok(Token(TokenKind::Boolean(false), from)),
            "null" => Ok(Token(TokenKind::Null, from)),
            _ => Ok(Token(TokenKind::Name(string), from)),
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

    fn scan_string(&mut self, start: usize) -> Result<Token<'a>, LexerError> {
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

                    return Ok(Token(TokenKind::String(string), start));
                }
                '\\' => {
                    if let Some(f) = from {
                        string.push_str(&self.src[f..i]);
                    }

                    from = None;
                    match self.scan_escaped_character() {
                        Some(c) => string.push(c),
                        None => return Err(LexerError(ErrorKind::InvalidEscapeSequence, i)),
                    }
                }
                // allowed source characters are /[\u0009\u000A\u000D\u0020-\uFFFF]/
                // in this case except LF (\u000A) and CR (\u000D)
                '\u{0000}'...'\u{008}' |
                '\u{000A}'...'\u{001F}' => {
                    return Err(LexerError(ErrorKind::UnexpectedCharacter, i))
                }
                _ => continue,
            }
        }

        Err(LexerError(ErrorKind::UnterminatedString, start))
    }

    fn scan_number(&mut self, i: usize) -> Result<Token<'a>, LexerError> {
        let from = i;
        let mut to = i + 1;

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
                Ok(float) => return Ok(Token(TokenKind::Float(float), i)),
                Err(_) => return Err(LexerError(ErrorKind::InvalidNumber, from)),
            }
        } else {
            match string.parse() {
                Ok(int) => return Ok(Token(TokenKind::Int(int), i)),
                Err(_) => return Err(LexerError(ErrorKind::InvalidNumber, from)),
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
        assert_eq!(lexer.scan(), Ok(Token(TokenKind::LeftBrace, 0)));
        assert_eq!(lexer.scan(), Ok(Token(TokenKind::RightBrace, 1)));
        assert_eq!(lexer.scan(), Ok(Token(TokenKind::EOF, 2)));
    }

    #[test]
    fn value_int() {
        assert_eq!(Lexer::new("0").scan(), Ok(Token(TokenKind::Int(0), 0)));
        assert_eq!(Lexer::new("-0").scan(), Ok(Token(TokenKind::Int(0), 0)));
        assert_eq!(Lexer::new("42").scan(), Ok(Token(TokenKind::Int(42), 0)));
        assert_eq!(Lexer::new("-42").scan(), Ok(Token(TokenKind::Int(-42), 0)));

        // skipping wrong number parts
        assert_eq!(Lexer::new("042").scan(), Ok(Token(TokenKind::Int(0), 0)));
        // assert_eq!(Lexer::new("042").scan(), unexpected(b"042"));
        assert_eq!(Lexer::new("-042").scan(), Ok(Token(TokenKind::Int(0), 0)));
        // assert_eq!(Lexer::new("-042").scan(), unexpected(b"042"));

        // max values
        assert_eq!(Lexer::new("2147483647").scan(),
                   Ok(Token(TokenKind::Int(2147483647), 0)));
        assert_eq!(Lexer::new("-2147483648").scan(),
                   Ok(Token(TokenKind::Int(-2147483648), 0)));

        // test overflow
        assert_eq!(Lexer::new("2147483648").scan(),
                   Err(LexerError(ErrorKind::InvalidNumber, 0)));
        assert_eq!(Lexer::new("-2147483649").scan(),
                   Err(LexerError(ErrorKind::InvalidNumber, 0)));
    }

    #[test]
    fn value_float() {
        assert_eq!(Lexer::new("1.0").scan(),
                   Ok(Token(TokenKind::Float(1.0), 0)));
        assert_eq!(Lexer::new("0.0").scan(),
                   Ok(Token(TokenKind::Float(0.0), 0)));
        assert_eq!(Lexer::new("0.04").scan(),
                   Ok(Token(TokenKind::Float(0.04), 0)));

        assert_eq!(Lexer::new("4.2e5").scan(),
                   Ok(Token(TokenKind::Float(420000.0), 0)));
        assert_eq!(Lexer::new("4.2E5").scan(),
                   Ok(Token(TokenKind::Float(420000.0), 0)));
        assert_eq!(Lexer::new("4.2e+5").scan(),
                   Ok(Token(TokenKind::Float(420000.0), 0)));
        assert_eq!(Lexer::new("4.2E+5").scan(),
                   Ok(Token(TokenKind::Float(420000.0), 0)));
        assert_eq!(Lexer::new("4.2e-5").scan(),
                   Ok(Token(TokenKind::Float(0.000042), 0)));
        assert_eq!(Lexer::new("4.2E-5").scan(),
                   Ok(Token(TokenKind::Float(0.000042), 0)));
        assert_eq!(Lexer::new("42e+5").scan(),
                   Ok(Token(TokenKind::Float(4200000.0), 0)));
        assert_eq!(Lexer::new("42E+5").scan(),
                   Ok(Token(TokenKind::Float(4200000.0), 0)));
        assert_eq!(Lexer::new("42e-5").scan(),
                   Ok(Token(TokenKind::Float(0.00042), 0)));
        assert_eq!(Lexer::new("42E-5").scan(),
                   Ok(Token(TokenKind::Float(0.00042), 0)));

        // skipping wrong number parts
        assert_eq!(Lexer::new("42.").scan(), Ok(Token(TokenKind::Int(42), 0)));
        assert_eq!(Lexer::new("42.0e").scan(),
                   Ok(Token(TokenKind::Float(42.0), 0)));
        assert_eq!(Lexer::new("42.0E").scan(),
                   Ok(Token(TokenKind::Float(42.0), 0)));
        assert_eq!(Lexer::new("42e+").scan(), Ok(Token(TokenKind::Int(42), 0)));
        assert_eq!(Lexer::new("42E-").scan(), Ok(Token(TokenKind::Int(42), 0)));

        // max values
        assert_eq!(Lexer::new("3.40282347e+38").scan(),
                   Ok(Token(TokenKind::Float(std::f32::MAX), 0)));
        assert_eq!(Lexer::new("-3.4028235e+38").scan(),
                   Ok(Token(TokenKind::Float(std::f32::MIN), 0)));

        // test overflow
        assert_eq!(Lexer::new("340282350000000000000000000000000000001").scan(),
                   Err(LexerError(ErrorKind::InvalidNumber, 0)));
        assert_eq!(Lexer::new("-340282350000000000000000000000000000001").scan(),
                   Err(LexerError(ErrorKind::InvalidNumber, 0)));
    }

    #[test]
    fn value_boolean() {
        assert_eq!(Lexer::new("true").scan(),
                   Ok(Token(TokenKind::Boolean(true), 0)));
        assert_eq!(Lexer::new("false").scan(),
                   Ok(Token(TokenKind::Boolean(false), 0)));
    }

    #[test]
    fn value_string() {
        assert_eq!(Lexer::new(r#"""#).scan(),
                   Err(LexerError(ErrorKind::UnterminatedString, 0)));
        assert_eq!(Lexer::new(r#""""#).scan(),
                   Ok(Token(TokenKind::String("".to_string()), 0)));
        assert_eq!(Lexer::new(r#""foobar""#).scan(),
                   Ok(Token(TokenKind::String("foobar".to_string()), 0)));
        assert_eq!(Lexer::new(r#""foo\"bar""#).scan(),
                   Ok(Token(TokenKind::String(r#"foo"bar"#.to_string()), 0)));
        assert_eq!(Lexer::new(r#""🦈""#).scan(),
                   Ok(Token(TokenKind::String(r#"🦈"#.to_string()), 0)));
        assert_eq!(Lexer::new(r#""a
            b""#)
                           .scan(),
                   Err(LexerError(ErrorKind::UnexpectedCharacter, 2)));
        assert_eq!(Lexer::new("\"\t\"").scan(),
                   Ok(Token(TokenKind::String("\t".to_string()), 0)));

        assert_eq!(Lexer::new(r#""\b""#).scan(),
                   Ok(Token(TokenKind::String("\u{0008}".to_string()), 0)));
        assert_eq!(Lexer::new(r#""\f""#).scan(),
                   Ok(Token(TokenKind::String("\u{000c}".to_string()), 0)));
        assert_eq!(Lexer::new(r#""\n""#).scan(),
                   Ok(Token(TokenKind::String("\n".to_string()), 0)));
        assert_eq!(Lexer::new(r#""\r""#).scan(),
                   Ok(Token(TokenKind::String("\r".to_string()), 0)));
        assert_eq!(Lexer::new(r#""\t""#).scan(),
                   Ok(Token(TokenKind::String("\t".to_string()), 0)));

        assert_eq!(Lexer::new("\"\\u2699\"").scan(),
                   Ok(Token(TokenKind::String("⚙".to_string()), 0)));
    }
}