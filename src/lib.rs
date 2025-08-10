use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write, write};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug)]
pub struct Error {
    error_kind: ErrorKind,
    position: usize,
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at position {}", self.error_kind, self.position)
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedToken(char),
    Incomplete,
    ParseFloatError(ParseFloatError),
    ParseIntError(ParseIntError),
}

impl From<ParseFloatError> for ErrorKind {
    fn from(value: ParseFloatError) -> Self {
        ErrorKind::ParseFloatError(value)
    }
}

impl From<ParseIntError> for ErrorKind {
    fn from(value: ParseIntError) -> Self {
        ErrorKind::ParseIntError(value)
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedToken(token) => write!(f, "unexpected token {token}"),
            ErrorKind::Incomplete => write!(f, "incomplete state"),
            ErrorKind::ParseFloatError(err) => write!(f, "{}", err),
            ErrorKind::ParseIntError(err) => write!(f, "{}", err),
        }
    }
}

pub fn parse(content: &str) -> Result<JsonValue, Error> {
    let mut parser = Parser::new(content);
    parser.next()
}

pub struct Parser<'a> {
    content: &'a str,
    read: usize,
}

impl<'a> Parser<'a> {
    pub fn new(content: &str) -> Parser {
        Parser { content, read: 0 }
    }

    pub fn next(&mut self) -> Result<JsonValue, Error> {
        self.parse_value().map_err(|error_kind| Error {
            error_kind,
            position: self.read,
        })
    }

    fn parse_value(&mut self) -> Result<JsonValue, ErrorKind> {
        match self.next_token().ok_or(ErrorKind::Incomplete)?? {
            Token::ObjectBegin => self.parse_object(),
            Token::ArrayBegin => self.parse_array(),
            Token::String => self.parse_string(),
            Token::Number => self.parse_number(),
            Token::True => self.parse_true(),
            Token::False => self.parse_false(),
            Token::Null => self.parse_null(),
            _ => Err(ErrorKind::UnexpectedToken(
                self.consumed().chars().last().unwrap(),
            )),
        }
    }

    fn next_token(&mut self) -> Option<Result<Token, ErrorKind>> {
        for (index, character) in self.remaining().char_indices() {
            if character.is_whitespace() {
                continue;
            }

            self.read += index + character.len_utf8();

            return Some(Token::try_from(character));
        }
        None
    }

    fn peek_next_token(&self) -> Option<Result<Token, ErrorKind>> {
        for character in self.remaining().chars() {
            if character.is_whitespace() {
                continue;
            }

            return Some(Token::try_from(character));
        }
        None
    }

    fn remaining(&self) -> &str {
        &self.content[self.read..]
    }

    fn consumed(&self) -> &str {
        &self.content[..self.read]
    }

    fn parse_true(&mut self) -> Result<JsonValue, ErrorKind> {
        self.expect_char('r')?;
        self.expect_char('u')?;
        self.expect_char('e')?;
        Ok(JsonValue::Bool(true))
    }

    fn parse_false(&mut self) -> Result<JsonValue, ErrorKind> {
        self.expect_char('a')?;
        self.expect_char('l')?;
        self.expect_char('s')?;
        self.expect_char('e')?;
        Ok(JsonValue::Bool(false))
    }

    fn parse_null(&mut self) -> Result<JsonValue, ErrorKind> {
        self.expect_char('u')?;
        self.expect_char('l')?;
        self.expect_char('l')?;
        Ok(JsonValue::Null)
    }

    fn parse_object(&mut self) -> Result<JsonValue, ErrorKind> {
        let mut object = HashMap::new();

        if let Some(Ok(Token::ObjectEnd)) = self.peek_next_token() {
            self.expect_token(Token::ObjectEnd)?;
            return Ok(JsonValue::Object(object));
        }

        self.expect_token(Token::String)?;
        let key = self.parse_key()?;

        self.expect_token(Token::Colon)?;
        let value = self.parse_value()?;

        object.insert(key, value);

        while Token::Comma == self.peek_next_token().ok_or(ErrorKind::Incomplete)?? {
            self.expect_token(Token::Comma)?;

            self.expect_token(Token::String)?;
            let key = self.parse_key()?;

            self.expect_token(Token::Colon)?;

            let value = self.parse_value()?;
            object.insert(key, value);
        }

        self.expect_token(Token::ObjectEnd)?;

        Ok(JsonValue::Object(object))
    }

    fn parse_array(&mut self) -> Result<JsonValue, ErrorKind> {
        let mut array = Vec::new();

        if let Some(Ok(Token::ArrayEnd)) = self.peek_next_token() {
            self.expect_token(Token::ArrayEnd)?;
            return Ok(JsonValue::Array(array));
        }

        let item = self.parse_value()?;
        array.push(item);
        while Token::Comma == self.peek_next_token().ok_or(ErrorKind::Incomplete)?? {
            self.expect_token(Token::Comma)?;

            let item = self.parse_value()?;
            array.push(item);
        }

        self.expect_token(Token::ArrayEnd)?;

        Ok(JsonValue::Array(array))
    }

    fn parse_string(&mut self) -> Result<JsonValue, ErrorKind> {
        self.parse_key().map(|string| JsonValue::String(string))
    }

    fn parse_key(&mut self) -> Result<String, ErrorKind> {
        let begin = self.read;
        loop {
            let candidate = self.remaining().char_indices().find(|(i, c)| {
                return *c == '"'
            });

            match candidate {
                Some((pos, character)) => self.read += pos + character.len_utf8(),
                None => {
                    self.read = self.content.len();
                    return Err(ErrorKind::Incomplete);
                }
            };
            if !self.last_consumed_char_is_escaped() {
                let string = String::from(&self.content[begin..self.read -1]);
                return Ok(string);
            }
        }
    }

    fn parse_number(&mut self) -> Result<JsonValue, ErrorKind> {
        let begin = self.read - 1;

        while let Some((index, character)) = self.remaining().char_indices().next() {
            if character.is_ascii_digit()
                || character == '.'
                || character == 'e'
                || character == 'E'
            {
                self.read += index + character.len_utf8();
            }else {break}
        }
        let number = String::from(&self.consumed()[begin..]);

        Ok(JsonValue::Number(number.parse()?))
    }

    fn expect_char(&mut self, want: char) -> Result<(), ErrorKind> {
        let (result, read) = match self.remaining().char_indices().next() {
            Some((i, c)) if c == want => (Ok(()), i + c.len_utf8()),
            Some((i, c)) => (Err(ErrorKind::UnexpectedToken(c)), i + c.len_utf8()),
            None => (Err(ErrorKind::Incomplete), 0),
        };
        self.read += read;
        result
    }

    fn expect_token(&mut self, want: Token) -> Result<(), ErrorKind> {
        match self.next_token() {
            Some(Ok(token)) if token == want => Ok(()),
            Some(Ok(_)) => Err(ErrorKind::UnexpectedToken(
                self.consumed()
                    .chars()
                    .last()
                    .expect("there should be at least one consumed char after reading a token"),
            )),
            Some(Err(err)) => Err(err),
            None => Err(ErrorKind::Incomplete),
        }
    }

    fn last_consumed_char_is_escaped(&self) -> bool {
        let mut is_escaped = false;
        while let Some('\\') = self.consumed().chars().rev().next() {
            is_escaped = !is_escaped;
        }

        is_escaped
    }
}

#[derive(Debug)]
pub enum JsonValue {
    String(String),
    Number(f64),
    Object(HashMap<String, JsonValue>),
    Array(Vec<JsonValue>),
    Bool(bool),
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    ObjectBegin,
    ObjectEnd,
    ArrayBegin,
    ArrayEnd,
    String,
    Number,
    True,
    False,
    Null,
    Colon,
    Comma,
}

impl TryFrom<char> for Token {
    type Error = ErrorKind;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        let token = match value {
            '{' => Token::ObjectBegin,
            '}' => Token::ObjectEnd,
            '[' => Token::ArrayBegin,
            ']' => Token::ArrayEnd,
            '\"' => Token::String,
            '-' => Token::Number,
            '0'..='9' => Token::Number,
            't' => Token::True,
            'f' => Token::False,
            'n' => Token::Null,
            ':' => Token::Colon,
            ',' => Token::Comma,
            _ => return Err(ErrorKind::UnexpectedToken(value)),
        };

        Ok(token)
    }
}
//
// #[derive(Debug)]
// enum Validator {
//     Object(ObjectValidator),
//     Array(ArrayValidator),
//     String(StringValidator),
//     Number(NumberValidator),
//     Bool(BoolValidator),
//     Null(NullValidator),
// }
// impl Validator {
//     fn new(token: Token) -> Result<Validator, Token> {
//         if let Ok(p) = NullValidator::Initial.next(token) {
//             Ok(Validator::Null(p))
//         } else if let Ok(p) = BoolValidator::Initial.next(token) {
//             Ok(Validator::Bool(p))
//         } else if let Ok(p) = NumberValidator::Initial.next(token) {
//             Ok(Validator::Number(p))
//         } else if let Ok(p) = StringValidator::Initial.next(token) {
//             Ok(Validator::String(p))
//         } else if let Ok(p) = ObjectValidator::Initial.next(token) {
//             Ok(Validator::Object(p))
//         } else if let Ok(p) = ArrayValidator::Initial.next(token) {
//             Ok(Validator::Array(p))
//         } else {
//             Err(token)
//         }
//     }
//
//     fn next(&self, token: Token) -> Result<Validator, Token> {
//         match self {
//             Validator::Object(p) => {
//                 let p = p.next(token)?;
//                 Ok(Validator::Object(p))
//             }
//             Validator::Array(p) => {
//                 let p = p.next(token)?;
//                 Ok(Validator::Array(p))
//             }
//             Validator::String(p) => {
//                 let p = p.next(token)?;
//                 Ok(Validator::String(p))
//             }
//             Validator::Number(p) => {
//                 let p = p.next(token)?;
//                 Ok(Validator::Number(p))
//             }
//             Validator::Bool(p) => {
//                 let p = p.next(token)?;
//                 Ok(Validator::Bool(p))
//             }
//             Validator::Null(p) => {
//                 let p = p.next(token)?;
//                 Ok(Validator::Null(p))
//             }
//         }
//     }
//     fn is_done(&self) -> Option<Token> {
//         match self {
//             Validator::Object(p) => p.is_done(),
//             Validator::Array(p) => p.is_done(),
//             Validator::String(p) => p.is_done(),
//             Validator::Number(p) => p.is_done(),
//             Validator::Bool(p) => p.is_done(),
//             Validator::Null(p) => p.is_done(),
//         }
//     }
// }
//
// pub fn validate(value: &str) -> bool {
//     let mut validators = Vec::new();
//     for c in value.chars() {
//         if c == ' ' || c == '\n' {
//             continue;
//         }
//         let token = match c {
//             '{' => Token::ObjectBegin,
//             '}' => Token::ObjectEnd,
//             ':' => Token::Colon,
//             ',' => Token::Comma,
//             '[' => Token::ArrayBegin,
//             ']' => Token::ArrayEnd,
//             '\\' => Token::Escape,
//             '"' => Token::String,
//             '0'..='9' => Token::Digit,
//             '-' => Token::Minus,
//             _ => Token::Character(c),
//         };
//         let mut current = match validators.pop() {
//             Some(current) => current,
//             None => match Validator::new(token) {
//                 Ok(new) => match new {
//                     Validator::Object(_) => Validator::Object(ObjectValidator::Initial),
//                     Validator::Array(_) => Validator::Array(ArrayValidator::Initial),
//                     _ => return false,
//                 },
//                 Err(_) => return false,
//             }, // match Validator::new(token) {
//                //     Ok(new) => {
//                //         validators.push(new);
//                //         continue;
//                //     }
//                //     Err(_) => return false,
//                // },
//                // },
//         };
//         // let mut current = validators
//         //     .pop()
//         //     .expect("Validator stack should not be empty");
//         while let Some(validated) = current.is_done() {
//             if let (Token::Number, Token::Digit) = (validated, token) {
//                 break;
//             }
//             current = match validators.pop() {
//                 Some(current) => current,
//                 None => return false,
//             };
//             current = match current.next(validated) {
//                 Ok(next) => next,
//                 Err(_) => return false,
//             }
//         }
//         match current.next(token) {
//             Ok(next) => validators.push(next),
//             Err(token) => match Validator::new(token) {
//                 Ok(new) => {
//                     validators.push(current);
//                     validators.push(new);
//                 }
//                 Err(_) => return false,
//             },
//         }
//     }
//     validators.len() == 1 && validators.last().unwrap().is_done().is_some()
// }
//
// #[derive(Debug)]
// enum ObjectValidator {
//     Initial,
//     Open,
//     Done,
//     Colon,
//     Comma,
//     Name,
//     Value,
// }
// impl ObjectValidator {
//     fn next(&self, token: Token) -> Result<ObjectValidator, Token> {
//         match self {
//             ObjectValidator::Open => match token {
//                 Token::ObjectEnd => Ok(ObjectValidator::Done),
//                 Token::String => Ok(ObjectValidator::Name),
//                 _ => Err(token),
//             },
//             ObjectValidator::Done => Err(token),
//             ObjectValidator::Colon => match token {
//                 Token::Object
//                 | Token::Array
//                 | Token::String
//                 | Token::Number
//                 | Token::Bool
//                 | Token::Null => Ok(ObjectValidator::Value),
//                 _ => Err(token),
//             },
//             ObjectValidator::Comma => match token {
//                 Token::String => Ok(ObjectValidator::Name),
//                 _ => Err(token),
//             },
//             ObjectValidator::Name => match token {
//                 Token::Colon => Ok(ObjectValidator::Colon),
//                 _ => Err(token),
//             },
//             ObjectValidator::Value => match token {
//                 Token::ObjectEnd => Ok(ObjectValidator::Done),
//                 Token::Comma => Ok(ObjectValidator::Comma),
//                 _ => Err(token),
//             },
//             ObjectValidator::Initial => match token {
//                 Token::ObjectBegin => Ok(ObjectValidator::Open),
//                 _ => Err(token),
//             },
//         }
//     }
//
//     fn is_done(&self) -> Option<Token> {
//         match self {
//             ObjectValidator::Done => Some(Token::Object),
//             _ => None,
//         }
//     }
// }
//
// #[derive(Debug)]
// enum ArrayValidator {
//     Initial,
//     Open,
//     Value,
//     Comma,
//     Done,
// }
// impl ArrayValidator {
//     fn next(&self, token: Token) -> Result<ArrayValidator, Token> {
//         match self {
//             ArrayValidator::Initial => match token {
//                 Token::ArrayBegin => Ok(ArrayValidator::Open),
//                 _ => Err(token),
//             },
//             ArrayValidator::Open => match token {
//                 Token::Object
//                 | Token::Array
//                 | Token::String
//                 | Token::Number
//                 | Token::Bool
//                 | Token::Null => Ok(ArrayValidator::Value),
//                 Token::ArrayEnd => Ok(ArrayValidator::Done),
//                 _ => Err(token),
//             },
//             ArrayValidator::Value => match token {
//                 Token::Comma => Ok(ArrayValidator::Comma),
//                 Token::ArrayEnd => Ok(ArrayValidator::Done),
//                 _ => Err(token),
//             },
//             ArrayValidator::Comma => match token {
//                 Token::Object
//                 | Token::Array
//                 | Token::String
//                 | Token::Number
//                 | Token::Bool
//                 | Token::Null => Ok(ArrayValidator::Value),
//                 _ => Err(token),
//             },
//             ArrayValidator::Done => Err(token),
//         }
//     }
//
//     fn is_done(&self) -> Option<Token> {
//         match self {
//             &ArrayValidator::Done => Some(Token::Array),
//             _ => None,
//         }
//     }
// }
//
// #[derive(Debug)]
// enum StringValidator {
//     Initial,
//     Open,
//     Done,
//     Character,
//     //TODO escape symbols
// }
// impl StringValidator {
//     fn next(&self, token: Token) -> Result<StringValidator, Token> {
//         match self {
//             StringValidator::Initial => match token {
//                 Token::String => Ok(StringValidator::Open),
//                 _ => Err(token),
//             },
//             StringValidator::Open => match token {
//                 Token::String => Ok(StringValidator::Done),
//                 _ => Ok(StringValidator::Character),
//             },
//             StringValidator::Character => match token {
//                 Token::String => Ok(StringValidator::Done),
//                 _ => Ok(StringValidator::Character),
//             },
//             StringValidator::Done => Err(token),
//         }
//     }
//
//     fn is_done(&self) -> Option<Token> {
//         match self {
//             StringValidator::Done => Some(Token::String),
//             _ => None,
//         }
//     }
// }
//
// #[derive(Debug)]
// enum NumberValidator {
//     Initial,
//     Minus,
//     Digit, //TODO Floating point and exponent
// }
//
// impl NumberValidator {
//     fn next(&self, token: Token) -> Result<NumberValidator, Token> {
//         match self {
//             NumberValidator::Initial => match token {
//                 Token::Digit => Ok(NumberValidator::Digit),
//                 Token::Minus => Ok(NumberValidator::Minus),
//                 _ => Err(token),
//             },
//             NumberValidator::Minus => match token {
//                 Token::Digit => Ok(NumberValidator::Digit),
//                 _ => Err(token),
//             },
//             NumberValidator::Digit => match token {
//                 Token::Digit => Ok(NumberValidator::Digit),
//                 _ => Err(token),
//             },
//         }
//     }
//
//     fn is_done(&self) -> Option<Token> {
//         match self {
//             NumberValidator::Digit => Some(Token::Number),
//             _ => None,
//         }
//     }
// }
//
// #[derive(Debug)]
// enum BoolValidator {
//     Initial,
//     T,
//     R,
//     U,
//     Done,
//     F,
//     A,
//     L,
//     S,
// }
// impl BoolValidator {
//     fn next(&self, token: Token) -> Result<BoolValidator, Token> {
//         match token {
//             Token::Character(c) => match self {
//                 BoolValidator::Initial => match c {
//                     't' => Ok(BoolValidator::T),
//                     'f' => Ok(BoolValidator::F),
//                     _ => Err(token),
//                 },
//                 BoolValidator::T => match c {
//                     'r' => Ok(BoolValidator::R),
//                     _ => Err(token),
//                 },
//                 BoolValidator::R => match c {
//                     'u' => Ok(BoolValidator::U),
//                     _ => Err(token),
//                 },
//                 BoolValidator::U => match c {
//                     'e' => Ok(BoolValidator::Done),
//                     _ => Err(token),
//                 },
//                 BoolValidator::Done => Err(token),
//                 BoolValidator::F => match c {
//                     'a' => Ok(BoolValidator::A),
//                     _ => Err(token),
//                 },
//                 BoolValidator::A => match c {
//                     'l' => Ok(BoolValidator::L),
//                     _ => Err(token),
//                 },
//                 BoolValidator::L => match c {
//                     's' => Ok(BoolValidator::S),
//                     _ => Err(token),
//                 },
//                 BoolValidator::S => match c {
//                     'e' => Ok(BoolValidator::Done),
//                     _ => Err(token),
//                 },
//             },
//             _ => Err(token),
//         }
//     }
//
//     fn is_done(&self) -> Option<Token> {
//         match self {
//             BoolValidator::Done => Some(Token::Bool),
//             _ => None,
//         }
//     }
// }
//
// #[derive(Debug)]
// enum NullValidator {
//     Initial,
//     N,
//     U,
//     L,
//     Done,
// }
// impl NullValidator {
//     fn next(&self, token: Token) -> Result<NullValidator, Token> {
//         match token {
//             Token::Character(c) => match self {
//                 NullValidator::Initial => match c {
//                     'n' => Ok(NullValidator::N),
//                     _ => Err(token),
//                 },
//                 NullValidator::N => match c {
//                     'u' => Ok(NullValidator::U),
//                     _ => Err(token),
//                 },
//                 NullValidator::U => match c {
//                     'l' => Ok(NullValidator::L),
//                     _ => Err(token),
//                 },
//                 NullValidator::L => match c {
//                     'l' => Ok(NullValidator::Done),
//                     _ => Err(token),
//                 },
//                 NullValidator::Done => Err(token),
//             },
//             _ => Err(token),
//         }
//     }
//     fn is_done(&self) -> Option<Token> {
//         match self {
//             NullValidator::Done => Some(Token::Null),
//             _ => None,
//         }
//     }
// }
// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn fail1() {
//         let input = r#"
//         "A JSON payload should be an object or array, not a string."
//        "#;
//         assert_eq!(validate(input), false);
//     }
//     #[test]
//     fn pass1() {
//         todo!("Add support for floating point and exponential numbers");
//         let input = r#"[
//     "JSON Test Pattern pass1",
//     {"object with 1 member":["array with 1 element"]},
//     {},
//     [],
//     -42,
//     true,
//     false,
//     null,
//     {
//         "integer": 1234567890,
//         "real": -9876.543210,
//         "e": 0.123456789e-12,
//         "E": 1.234567890E+34,
//         "":  23456789012E66,
//         "zero": 0,
//         "one": 1,
//         "space": " ",
//         "quote": "\"",
//         "backslash": "\\",
//         "controls": "\b\f\n\r\t",
//         "slash": "/ & \/",
//         "alpha": "abcdefghijklmnopqrstuvwyz",
//         "ALPHA": "ABCDEFGHIJKLMNOPQRSTUVWYZ",
//         "digit": "0123456789",
//         "0123456789": "digit",
//         "special": "`1~!@#$%^&*()_+-={':[,]}|;.</>?",
//         "hex": "\u0123\u4567\u89AB\uCDEF\uabcd\uef4A",
//         "true": true,
//         "false": false,
//         "null": null,
//         "array":[  ],
//         "object":{  },
//         "address": "50 St. James Street",
//         "url": "http://www.JSON.org/",
//         "comment": "// /* <!-- --",
//         "\# -- --> */": " ",
//         " s p a c e d " :[1,2 , 3
//
// ,
//
// 4 , 5        ,          6           ,7        ],"compact":[1,2,3,4,5,6,7],
//         "jsontext": "{\"object with 1 member\":[\"array with 1 element\"]}",
//         "quotes": "&#34; \u0022 %22 0x22 034 &#x22;",
//         "\/\\\"\uCAFE\uBABE\uAB98\uFCDE\ubcda\uef4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
// : "A key can be any string"
//     },
//     0.5 ,98.6
// ,
// 99.44
// ,
//
// 1066,
// 1e1,
// 0.1e1,
// 1e-1,
// 1e00,2e+00,2e-00
// ,"rosebud"]
//        "#;
//         let actual = validate(input);
//         assert_eq!(actual, true);
//     }
//     #[test]
//     fn pass2() {
//         let input = r#"
// [[[[[[[[[[[[[[[[[[["Not too deep"]]]]]]]]]]]]]]]]]]]
//        "#;
//         let actual = validate(input);
//         assert_eq!(actual, true);
//     }
// }
