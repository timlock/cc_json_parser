use std::any::Any;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::num::ParseFloatError;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub error_kind: ErrorKind,
    pub position: usize,
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at position {}", self.error_kind, self.position)
    }
}

impl std::error::Error for Error {}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    UnexpectedToken(char),
    Incomplete,
    ParseFloatError(ParseFloatError),
    InvalidType {
        key: String,
        want: &'static str,
        got: &'static str,
    },
    UnexpectedType(&'static str),
}

impl From<ParseFloatError> for ErrorKind {
    fn from(value: ParseFloatError) -> Self {
        ErrorKind::ParseFloatError(value)
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedToken(token) => write!(f, "unexpected token {token}"),
            ErrorKind::Incomplete => write!(f, "incomplete state"),
            ErrorKind::ParseFloatError(err) => write!(f, "{}", err),
            ErrorKind::InvalidType { key, want, got } => {
                write!(f, "field '{}' is not of type {} but {}", key, want, got)
            }
            ErrorKind::UnexpectedType(want) => write!(f, "expected type {want}"),
        }
    }
}

pub trait PParser {
    fn parse_string(&mut self) -> Option<Result<String, ErrorKind>>;
    fn parse_f64(&mut self) -> Option<Result<f64, ErrorKind>>;

    fn parse_bool(&mut self) -> Option<Result<bool, ErrorKind>>;
}

pub trait Parsable {
    fn parse(&mut self, parser: &mut dyn PParser) -> Result<(), String>;
}

pub trait FromJson {
    fn fields(&self) -> Vec<(&str)>;
    fn set(&mut self, key: &str, value: JsonValue) -> Result<(), ErrorKind>;
}

pub enum UnmarshalError {
    InvalidType(&'static str),
    InvalidFieldType(&'static str),
    InvalidValueType(&'static str),
}

pub trait Unmarshal: Any {
    fn set(&mut self, value: &dyn Unmarshal) -> Result<(), UnmarshalError>;
    fn push(&mut self, value: &dyn Unmarshal) -> Result<(), UnmarshalError>;
    fn set_field(
        &mut self,
        key: &dyn Unmarshal,
        value: &dyn Unmarshal,
    ) -> Result<(), UnmarshalError>;
}

#[derive(Debug, PartialEq)]
pub enum JsonValue {
    String(String),
    Number(f64),
    Object(BTreeMap<String, JsonValue>),
    Array(Vec<JsonValue>),
    Bool(bool),
    Null,
}

impl JsonValue {
    pub fn value_type(&self) -> &'static str {
        match self {
            JsonValue::String(_) => "string",
            JsonValue::Number(_) => "number",
            JsonValue::Object(_) => "object",
            JsonValue::Array(_) => "array",
            JsonValue::Bool(_) => "bool",
            JsonValue::Null => "null",
        }
    }
}

impl TryFrom<JsonValue> for String {
    type Error = (&'static str, &'static str);

    fn try_from(value: JsonValue) -> Result<Self, Self::Error> {
        match value {
            JsonValue::String(string) => Ok(string),
            _ => Err(("string", value.value_type())),
        }
    }
}

impl From<String> for JsonValue {
    fn from(value: String) -> Self {
        JsonValue::String(value)
    }
}

impl From<f64> for JsonValue {
    fn from(value: f64) -> Self {
        JsonValue::Number(value)
    }
}

impl From<BTreeMap<String, JsonValue>> for JsonValue {
    fn from(value: BTreeMap<String, JsonValue>) -> Self {
        JsonValue::Object(value)
    }
}

impl From<Vec<JsonValue>> for JsonValue {
    fn from(value: Vec<JsonValue>) -> Self {
        JsonValue::Array(value)
    }
}
impl From<bool> for JsonValue {
    fn from(value: bool) -> Self {
        JsonValue::Bool(value)
    }
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

pub fn parse(content: &str) -> Result<JsonValue, Error> {
    let mut parser = Parser::new(content);
    parser.next()
}

pub fn parse_object(content: &str, want: &mut BTreeMap<&str, JsonValue>) -> Result<(), Error> {
    todo!();
    // let mut parser = Parser::new(content);
    // parser.parse_object(want)
}

pub fn parse_any(content: &str, want: &mut dyn FromJson) -> Result<(), Error> {
    let iter = want.fields();
    let mut map = BTreeMap::new();
    for name in iter {
        map.insert(String::from(name), JsonValue::Null);
    }

    let mut parser = Parser::new(content);
    parser.parse_object(&mut map)?;

    for (field, value) in map {
        want.set(&field, value).map_err(|error_kind| Error {
            error_kind,
            position: 0,
        })?;
    }

    Ok(())
}

pub fn incremental_parse_parsable<T>(src: &str, dst: &mut T) -> Result<(), String>
where
    T: Parsable,
{
    let mut parser = IncrementalParser::new(src);
    dst.parse(&mut parser)
}

impl<'a> PParser for IncrementalParser<'a> {
    fn parse_string(&mut self) -> Option<Result<String, ErrorKind>> {
        match self.next_value()? {
            Ok(JsonValue::String(string)) => Some(Ok(string)),
            Ok(other) => Some(Err(ErrorKind::UnexpectedType("string"))),
            Err(err) => Some(Err(err)),
        }
    }

    fn parse_f64(&mut self) -> Option<Result<f64, ErrorKind>> {
        match self.next_value()? {
            Ok(JsonValue::Number(number)) => Some(Ok(number)),
            Ok(other) => Some(Err(ErrorKind::UnexpectedType("number"))),
            Err(err) => Some(Err(err)),
        }
    }

    fn parse_bool(&mut self) -> Option<Result<bool, ErrorKind>> {
        match self.next_value()? {
            Ok(JsonValue::Bool(bool)) => Some(Ok(bool)),
            Ok(other) => Some(Err(ErrorKind::UnexpectedType("bool"))),
            Err(err) => Some(Err(err)),
        }
    }
}

#[derive(Clone, Copy)]
struct ObjectValidator {
    state: ObjectValidatorState,
}

impl ObjectValidator {
    fn apply(&mut self, token: &Token) -> Result<(), ErrorKind> {
        self.state = self.state.apply(token)?;
        Ok(())
    }
}

#[derive(Clone, Copy)]
enum ObjectValidatorState {
    Begin,
    Key,
    Colon,
    Value,
    Comma,
    End,
}

impl ObjectValidatorState {
    fn apply(self, token: &Token) -> Result<ObjectValidatorState, ErrorKind> {
        match self {
            ObjectValidatorState::Begin => match token {
                Token::ObjectEnd => Ok(ObjectValidatorState::End),
                Token::String => Ok(ObjectValidatorState::Key),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ObjectValidatorState::Key => match token {
                Token::Colon => Ok(ObjectValidatorState::Colon),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ObjectValidatorState::Colon => match token {
                Token::ObjectBegin
                | Token::ArrayBegin
                | Token::String
                | Token::Number
                | Token::True
                | Token::False
                | Token::Null => Ok(ObjectValidatorState::Value),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ObjectValidatorState::Value => match token {
                Token::Comma => Ok(ObjectValidatorState::Comma),
                Token::ObjectEnd => Ok(ObjectValidatorState::End),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ObjectValidatorState::Comma => match token {
                Token::String => Ok(ObjectValidatorState::Key),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ObjectValidatorState::End => {
                panic!("Object should not contain more tokens after the closing brace")
            }
        }
    }
}

impl Default for ObjectValidator {
    fn default() -> Self {
        Self {
            state: ObjectValidatorState::Begin,
        }
    }
}

#[derive(Clone, Copy)]
struct ArrayValidator {
    state: ArrayValidatorState,
}

impl ArrayValidator {
    fn apply(&mut self, token: &Token) -> Result<(), ErrorKind> {
        self.state = self.state.apply(token)?;
        Ok(())
    }
}

#[derive(Clone, Copy)]
enum ArrayValidatorState {
    Begin,
    Value,
    Comma,
    End,
}

impl ArrayValidatorState {
    fn apply(self, token: &Token) -> Result<ArrayValidatorState, ErrorKind> {
        match self {
            ArrayValidatorState::Begin => match token {
                Token::ArrayEnd => Ok(ArrayValidatorState::End),
                Token::ObjectBegin
                | Token::ArrayBegin
                | Token::String
                | Token::Number
                | Token::True
                | Token::False
                | Token::Null => Ok(ArrayValidatorState::Value),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ArrayValidatorState::Value => match token {
                Token::Comma => Ok(ArrayValidatorState::Comma),
                Token::ArrayEnd => Ok(ArrayValidatorState::End),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ArrayValidatorState::Comma => match token {
                Token::ObjectBegin
                | Token::ArrayBegin
                | Token::String
                | Token::Number
                | Token::True
                | Token::False
                | Token::Null => Ok(ArrayValidatorState::Value),
                _ => Err(ErrorKind::UnexpectedToken(' ')),
            },
            ArrayValidatorState::End => {
                panic!("Object should not contain more tokens after the closing brace")
            }
        }
    }
}

impl Default for ArrayValidator {
    fn default() -> Self {
        Self {
            state: ArrayValidatorState::Begin,
        }
    }
}

enum Validator {
    Object(ObjectValidator),
    Array(ArrayValidator),
}

pub struct IncrementalParser<'a> {
    inner: Parser<'a>,
    validator_stack: Vec<Validator>,
}

impl<'a> IncrementalParser<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            inner: Parser::new(content),
            validator_stack: Vec::new(),
        }
    }

    fn next_value(&mut self) -> Option<Result<JsonValue, ErrorKind>> {
        if self.validator_stack.is_empty() {
            let next_token = self.inner.next_token()?;
            let next_token = match next_token {
                Ok(token) => token,
                Err(err) => return Some(Err(err)),
            };

            let validator = match next_token {
                Token::ObjectBegin => Validator::Object(ObjectValidator::default()),
                Token::ArrayBegin => Validator::Array(ArrayValidator::default()),
                Token::String => return Some(self.inner.expect_string().map(JsonValue::from)),
                Token::Number => return Some(self.inner.expect_number().map(JsonValue::from)),
                Token::True => return Some(self.inner.expect_true().map(JsonValue::from)),
                Token::False => return Some(self.inner.expect_false().map(JsonValue::from)),
                Token::Null => return Some(self.inner.expect_null()),
                _ => {
                    return Some(Err(ErrorKind::UnexpectedToken(
                        self.inner.consumed().chars().last().unwrap(),
                    )));
                }
            };

            self.validator_stack.push(validator);
        }

        loop {
            let token = match self.next_container_value() {
                Ok(token) => token,
                Err(err) => return Some(Err(err)),
            };

            match token {
                Token::ObjectBegin => self
                    .validator_stack
                    .push(Validator::Object(ObjectValidator::default())),
                Token::ArrayBegin => self
                    .validator_stack
                    .push(Validator::Array(ArrayValidator::default())),
                Token::String => return Some(self.inner.expect_string().map(JsonValue::from)),
                Token::Number => return Some(self.inner.expect_number().map(JsonValue::from)),
                Token::True => return Some(self.inner.expect_true().map(JsonValue::from)),
                Token::False => return Some(self.inner.expect_false().map(JsonValue::from)),
                Token::Null => return Some(self.inner.expect_null()),

                Token::ObjectEnd => {
                    self.validator_stack.pop();
                    return None;
                }
                Token::ArrayEnd => {
                    self.validator_stack.pop();
                    return None;
                }
                _ => panic!("next_object_value should not return {token:?}"),
            }
        }
    }

    fn next_container_value(&mut self) -> Result<Token, ErrorKind> {
        loop {
            let token = self.inner.next_token().ok_or(ErrorKind::Incomplete)??;

            let validator = self
                .validator_stack
                .last_mut()
                .ok_or(ErrorKind::Incomplete)?;

            match validator {
                Validator::Object(validator_state) => {
                    validator_state.apply(&token)?;
                    match token {
                        Token::ObjectBegin
                        | Token::ObjectEnd
                        | Token::ArrayBegin
                        | Token::String
                        | Token::Number
                        | Token::True
                        | Token::False
                        | Token::Null => return Ok(token),
                        Token::Colon | Token::Comma => {
                            // do nothing, we are only interested in values
                        }
                        _ => panic!(
                            "Should not be possible that an object validator validates a {token:?}"
                        ),
                    }
                }
                Validator::Array(validator_state) => {
                    validator_state.apply(&token)?;
                    match token {
                        Token::ObjectBegin
                        | Token::ArrayEnd
                        | Token::ArrayBegin
                        | Token::String
                        | Token::Number
                        | Token::True
                        | Token::False
                        | Token::Null => return Ok(token),
                        Token::Comma => {
                            // do nothing, we are only interested in values
                        }
                        _ => panic!(
                            "Should not be possible that an array validator validates a {token:?}"
                        ),
                    }
                }
            }
        }
    }
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
        self.expect_value().map_err(|error_kind| Error {
            error_kind,
            position: self.read,
        })
    }

    pub fn parse_object(&mut self, want: &mut BTreeMap<String, JsonValue>) -> Result<(), Error> {
        self.expect_token(Token::ObjectBegin)
            .map_err(|error_kind| Error {
                error_kind,
                position: self.read,
            })?;
        let mut got = self.expect_object().map_err(|error_kind| Error {
            error_kind,
            position: self.read,
        })?;

        for (key, value) in want.iter_mut() {
            if let Some(got_value) = got.remove(key) {
                *value = got_value
            }
        }

        Ok(())
    }

    fn expect_value(&mut self) -> Result<JsonValue, ErrorKind> {
        match self.next_token().ok_or(ErrorKind::Incomplete)?? {
            Token::ObjectBegin => self.expect_object().map(JsonValue::from),
            Token::ArrayBegin => self.expect_array().map(JsonValue::from),
            Token::String => self.expect_string().map(JsonValue::from),
            Token::Number => self.expect_number().map(JsonValue::from),
            Token::True => self.expect_true().map(JsonValue::from),
            Token::False => self.expect_false().map(JsonValue::from),
            Token::Null => self.expect_null(),
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

    fn expect_true(&mut self) -> Result<bool, ErrorKind> {
        self.expect_char('r')?;
        self.expect_char('u')?;
        self.expect_char('e')?;
        Ok(true)
    }

    fn expect_false(&mut self) -> Result<bool, ErrorKind> {
        self.expect_char('a')?;
        self.expect_char('l')?;
        self.expect_char('s')?;
        self.expect_char('e')?;
        Ok(false)
    }

    fn expect_null(&mut self) -> Result<JsonValue, ErrorKind> {
        self.expect_char('u')?;
        self.expect_char('l')?;
        self.expect_char('l')?;
        Ok(JsonValue::Null)
    }

    fn expect_object(&mut self) -> Result<BTreeMap<String, JsonValue>, ErrorKind> {
        let mut object = BTreeMap::new();

        if let Some(Ok(Token::ObjectEnd)) = self.peek_next_token() {
            self.expect_token(Token::ObjectEnd)?;
            return Ok(object);
        }

        loop {
            self.expect_token(Token::String)?;
            let key = self.expect_string()?;

            self.expect_token(Token::Colon)?;
            let value = self.expect_value()?;
            object.insert(key, value);

            if Token::Comma != self.peek_next_token().ok_or(ErrorKind::Incomplete)?? {
                break;
            }
            self.expect_token(Token::Comma)?;
        }

        self.expect_token(Token::ObjectEnd)?;

        Ok(object)
    }

    fn expect_array(&mut self) -> Result<Vec<JsonValue>, ErrorKind> {
        let mut array = Vec::new();

        if let Some(Ok(Token::ArrayEnd)) = self.peek_next_token() {
            self.expect_token(Token::ArrayEnd)?;
            return Ok(array);
        }

        loop {
            let item = self.expect_value()?;
            array.push(item);

            if Token::Comma != self.peek_next_token().ok_or(ErrorKind::Incomplete)?? {
                break;
            }
            self.expect_token(Token::Comma)?;
        }

        self.expect_token(Token::ArrayEnd)?;

        Ok(array)
    }

    fn expect_string(&mut self) -> Result<String, ErrorKind> {
        let begin = self.read;
        loop {
            let candidate = self
                .remaining()
                .char_indices()
                .find(|(i, c)| return *c == '"');

            match candidate {
                Some((pos, character)) => self.read += pos + character.len_utf8(),
                None => {
                    self.read = self.content.len();
                    return Err(ErrorKind::Incomplete);
                }
            };
            if !self.last_consumed_char_is_escaped() {
                let string = String::from(&self.content[begin..self.read - 1]);
                return Ok(string);
            }
        }
    }

    fn expect_number(&mut self) -> Result<f64, ErrorKind> {
        let begin = self.read - 1;

        let mut iter = self.remaining().chars();
        let mut read = 0;

        while let Some(character) = iter.next() {
            if character.is_ascii_digit()
                || character == '+'
                || character == '-'
                || character == '.'
                || character == 'e'
                || character == 'E'
            {
                read += character.len_utf8();
            } else {
                break;
            }
        }
        self.read += read;
        let number = String::from(&self.consumed()[begin..]);

        Ok(number.parse()?)
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
        let mut iter = self.consumed().chars().rev().skip(1);
        while let Some('\\') = iter.next() {
            is_escaped = !is_escaped;
        }

        is_escaped
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pass1() -> Result<(), Box<dyn std::error::Error>> {
        let input = r#"[
    "JSON Test Pattern pass1",
    {"object with 1 member":["array with 1 element"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        "integer": 1234567890,
        "real": -9876.543210,
        "e": 0.123456789e-12,
        "E": 1.234567890E+34,
        "":  23456789012E66,
        "zero": 0,
        "one": 1,
        "space": " ",
        "quote": "\"",
        "backslash": "\\",
        "controls": "\b\f\n\r\t",
        "slash": "/ & \/",
        "alpha": "abcdefghijklmnopqrstuvwyz",
        "ALPHA": "ABCDEFGHIJKLMNOPQRSTUVWYZ",
        "digit": "0123456789",
        "0123456789": "digit",
        "special": "`1~!@#$%^&*()_+-={':[,]}|;.</>?",
        "hex": "\u0123\u4567\u89AB\uCDEF\uabcd\uef4A",
        "true": true,
        "false": false,
        "null": null,
        "array":[  ],
        "object":{  },
        "address": "50 St. James Street",
        "url": "http://www.JSON.org/",
        "comment": "// /* <!-- --",
        "\# -- --> */": " ",
        " s p a c e d " :[1,2 , 3

,

4 , 5        ,          6           ,7        ],"compact":[1,2,3,4,5,6,7],
        "jsontext": "{\"object with 1 member\":[\"array with 1 element\"]}",
        "quotes": "&#34; \u0022 %22 0x22 034 &#x22;",
        "\/\\\"\uCAFE\uBABE\uAB98\uFCDE\ubcda\uef4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
: "A key can be any string"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,"rosebud"]
       "#;
        let got = parse(input)?;
        let want = JsonValue::Array(vec![
            JsonValue::String(String::from("JSON Test Pattern pass1")),
            JsonValue::Object(BTreeMap::from([(
                String::from("object with 1 member"),
                JsonValue::Array(vec![JsonValue::String(String::from(
                    "array with 1 element",
                ))]),
            )])),
            JsonValue::Object(BTreeMap::new()),
            JsonValue::Array(Vec::new()),
            JsonValue::Number(-42.),
            JsonValue::Bool(true),
            JsonValue::Bool(false),
            JsonValue::Null,
            JsonValue::Object(BTreeMap::from([
                (String::from("integer"), JsonValue::Number(1234567890.)),
                (String::from("real"), JsonValue::Number(-9876.543210)),
                (String::from("e"), JsonValue::Number(0.123456789e-12)),
                (String::from("E"), JsonValue::Number(1.234567890E+34)),
                (String::from(""), JsonValue::Number(23456789012E66)),
                (String::from("zero"), JsonValue::Number(0.)),
                (String::from("one"), JsonValue::Number(1.)),
                (String::from("space"), JsonValue::String(String::from(" "))),
                (
                    String::from("quote"),
                    JsonValue::String(String::from("\\\"")),
                ),
                (
                    String::from("backslash"),
                    JsonValue::String(String::from("\\\\")),
                ),
                (
                    String::from("controls"),
                    JsonValue::String(String::from("\\b\\f\\n\\r\\t")),
                ),
                (
                    String::from("slash"),
                    JsonValue::String(String::from("/ & \\/")),
                ),
                (
                    String::from("alpha"),
                    JsonValue::String(String::from("abcdefghijklmnopqrstuvwyz")),
                ),
                (
                    String::from("ALPHA"),
                    JsonValue::String(String::from("ABCDEFGHIJKLMNOPQRSTUVWYZ")),
                ),
                (
                    String::from("digit"),
                    JsonValue::String(String::from("0123456789")),
                ),
                (
                    String::from("0123456789"),
                    JsonValue::String(String::from("digit")),
                ),
                (
                    String::from("special"),
                    JsonValue::String(String::from("`1~!@#$%^&*()_+-={':[,]}|;.</>?")),
                ),
                (
                    String::from("hex"),
                    JsonValue::String(String::from("\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A")),
                ),
                (String::from("true"), JsonValue::Bool(true)),
                (String::from("false"), JsonValue::Bool(false)),
                (String::from("null"), JsonValue::Null),
                (String::from("array"), JsonValue::Array(Vec::new())),
                (String::from("object"), JsonValue::Object(BTreeMap::new())),
                (
                    String::from("address"),
                    JsonValue::String(String::from("50 St. James Street")),
                ),
                (
                    String::from("url"),
                    JsonValue::String(String::from("http://www.JSON.org/")),
                ),
                (
                    String::from("comment"),
                    JsonValue::String(String::from("// /* <!-- --")),
                ),
                (
                    String::from("\\# -- --> */"),
                    JsonValue::String(String::from(" ")),
                ),
                (
                    String::from(" s p a c e d "),
                    JsonValue::Array(vec![
                        JsonValue::Number(1.),
                        JsonValue::Number(2.),
                        JsonValue::Number(3.),
                        JsonValue::Number(4.),
                        JsonValue::Number(5.),
                        JsonValue::Number(6.),
                        JsonValue::Number(7.),
                    ]),
                ),
                (
                    String::from("compact"),
                    JsonValue::Array(vec![
                        JsonValue::Number(1.),
                        JsonValue::Number(2.),
                        JsonValue::Number(3.),
                        JsonValue::Number(4.),
                        JsonValue::Number(5.),
                        JsonValue::Number(6.),
                        JsonValue::Number(7.),
                    ]),
                ),
                (
                    String::from("jsontext"),
                    JsonValue::String(String::from(
                        "{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}",
                    )),
                ),
                (
                    String::from("quotes"),
                    JsonValue::String(String::from("&#34; \\u0022 %22 0x22 034 &#x22;")),
                ),
                (
                    String::from(
                        "\\/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?",
                    ),
                    JsonValue::String(String::from("A key can be any string")),
                ),
            ])),
            JsonValue::Number(0.5),
            JsonValue::Number(98.6),
            JsonValue::Number(99.44),
            JsonValue::Number(1066.),
            JsonValue::Number(1e1),
            JsonValue::Number(0.1e1),
            JsonValue::Number(1e-1),
            JsonValue::Number(1e00),
            JsonValue::Number(2e+00),
            JsonValue::Number(2e-00),
            JsonValue::String(String::from("rosebud")),
        ]);
        assert_eq!(want, got);
        Ok(())
    }
    #[test]
    fn pass2() -> Result<(), Box<dyn std::error::Error>> {
        let input = r#"
    [[[[[[[[[[[[[[[[[[["Not too deep"]]]]]]]]]]]]]]]]]]]
           "#;
        let got = parse(input)?;
        let want = JsonValue::Array(vec![JsonValue::Array(vec![JsonValue::Array(vec![
            JsonValue::Array(vec![JsonValue::Array(vec![JsonValue::Array(vec![
                JsonValue::Array(vec![JsonValue::Array(vec![JsonValue::Array(vec![
                    JsonValue::Array(vec![JsonValue::Array(vec![JsonValue::Array(vec![
                        JsonValue::Array(vec![JsonValue::Array(vec![JsonValue::Array(vec![
                            JsonValue::Array(vec![JsonValue::Array(vec![JsonValue::Array(vec![
                                JsonValue::Array(vec![JsonValue::String(String::from(
                                    "Not too deep",
                                ))]),
                            ])])]),
                        ])])]),
                    ])])]),
                ])])]),
            ])])]),
        ])])]);
        assert_eq!(want, got);
        Ok(())
    }

    #[test]
    fn test_parse_object() -> Result<(), Box<dyn std::error::Error>> {
        let input = r#"
        {
          "key": "value",
          "key2": "value"
        }
       "#;
        let mut got = BTreeMap::from([("key", JsonValue::Null), ("key2", JsonValue::Null)]);
        parse_object(input, &mut got)?;

        let want = BTreeMap::from([
            ("key", JsonValue::String(String::from("value"))),
            ("key2", JsonValue::String(String::from("value"))),
        ]);
        assert_eq!(want, got);
        Ok(())
    }

    #[derive(PartialEq, Debug)]
    struct ParseValueTest {
        key: String,
        key2: String,
        nested: Nested,
    }

    #[derive(PartialEq, Debug, Clone, Default)]
    struct Nested {
        number: i32,
    }
    impl Unmarshal for Nested {
        fn set(&mut self, value: &dyn Unmarshal) -> Result<(), UnmarshalError> {
            Err(UnmarshalError::InvalidType("Nested"))
        }

        fn push(&mut self, value: &dyn Unmarshal) -> Result<(), UnmarshalError> {
            Err(UnmarshalError::InvalidType("Nested"))
        }

        fn set_field(
            &mut self,
            key: &dyn Unmarshal,
            value: &dyn Unmarshal,
        ) -> Result<(), UnmarshalError> {
            let key = (key as &dyn Any)
                .downcast_ref::<String>()
                .ok_or(UnmarshalError::InvalidFieldType("string"))?;
            match key.as_str() {
                "number" => {
                    self.number = (value as &dyn Any)
                        .downcast_ref::<f64>()
                        .ok_or(UnmarshalError::InvalidValueType("i32"))?
                        .to_owned() as i32;
                    Ok(())
                }
                _ => Ok(()),
            }
        }
    }

    impl Unmarshal for ParseValueTest {
        fn set(&mut self, _: &dyn Unmarshal) -> Result<(), UnmarshalError> {
            Err(UnmarshalError::InvalidType("ParseValueTest"))
        }

        fn push(&mut self, _: &dyn Unmarshal) -> Result<(), UnmarshalError> {
            Err(UnmarshalError::InvalidType("ParseValueTest"))
        }

        fn set_field(
            &mut self,
            key: &dyn Unmarshal,
            value: &dyn Unmarshal,
        ) -> Result<(), UnmarshalError> {
            let key = (key as &dyn Any)
                .downcast_ref::<&str>()
                .ok_or(UnmarshalError::InvalidFieldType("&str"))?;
            match *key {
                "key" => {
                    self.key = (value as &dyn Any)
                        .downcast_ref::<String>()
                        .ok_or(UnmarshalError::InvalidValueType("String"))?
                        .to_owned();
                    Ok(())
                }
                "key2" => {
                    self.key2 = (value as &dyn Any)
                        .downcast_ref::<String>()
                        .ok_or(UnmarshalError::InvalidValueType("String"))?
                        .to_owned();
                    Ok(())
                }
                "nested" => {
                    self.nested = (value as &dyn Any)
                        .downcast_ref::<Nested>()
                        .ok_or(UnmarshalError::InvalidValueType("Nested"))?
                        .to_owned();
                    Ok(())
                }
                _ => Ok(()),
            }
        }
    }

    impl FromJson for ParseValueTest {
        fn fields(&self) -> Vec<&str> {
            vec!["key", "key2"]
        }

        fn set(&mut self, key: &str, value: JsonValue) -> Result<(), ErrorKind> {
            match key {
                "key" => {
                    self.key = JsonValue::try_into(value).map_err(|(want, got)| {
                        ErrorKind::InvalidType {
                            key: String::from(key),
                            want,
                            got,
                        }
                    })?
                }
                "key2" => {
                    self.key2 = JsonValue::try_into(value).map_err(|(want, got)| {
                        ErrorKind::InvalidType {
                            key: String::from(key),
                            want,
                            got,
                        }
                    })?
                }
                _ => return Err(ErrorKind::Incomplete),
            };
            Ok(())
        }
    }

    #[test]
    fn test_parse_any() -> Result<(), Box<dyn std::error::Error>> {
        let input = r#"
        {
          "key": "value",
          "key2": "value"
        }
       "#;
        let mut got = ParseValueTest {
            key: String::new(),
            key2: String::new(),
            nested: Default::default(),
        };
        parse_any(input, &mut got)?;

        let want = ParseValueTest {
            key: String::from("value"),
            key2: String::from("value"),
            nested: Default::default(),
        };
        assert_eq!(want, got);
        Ok(())
    }

    impl Parsable for Nested {
        fn parse(&mut self, parser: &mut dyn PParser) -> Result<(), String> {
            while let Some(field_name) = parser.parse_string() {
                match field_name.map_err(|err| err.to_string())?.as_str() {
                    "number" => {
                        self.number = parser
                            .parse_f64()
                            .expect("expected value")
                            .map_err(|err| err.to_string())?
                            as i32
                    }
                    _ => return Err(String::from("unknown field")),
                }
            }
            Ok(())
        }
    }
    impl Parsable for ParseValueTest {
        fn parse(&mut self, parser: &mut dyn PParser) -> Result<(), String> {
            while let Some(field_name) = parser.parse_string() {
                match field_name.map_err(|err| err.to_string())?.as_str() {
                    "key" => {
                        self.key = parser
                            .parse_string()
                            .expect("expected value")
                            .map_err(|err| err.to_string())?
                    }
                    "key2" => {
                        self.key2 = parser
                            .parse_string()
                            .expect("expected value")
                            .map_err(|err| err.to_string())?
                    }
                    "nested" => self.nested.parse(parser)?,
                    _ => return Err(String::from("unknown field")),
                }
            }
            Ok(())
        }
    }

    impl<T> Parsable for Vec<T>
    where
        T: Parsable + Default,
    {
        fn parse(&mut self, parser: &mut dyn PParser) -> Result<(), String> {
            loop {
                let mut base = T::default();

                match base.parse(parser) {
                    Ok(_) => self.push(base),
                    Err(err) => return Ok(()), //TODO distinguish between error and no more values
                }
            }
        }
    }

    #[test]
    fn test_parse_parsable() -> Result<(), Box<dyn std::error::Error>> {
        let input = r#"
        {
          "key": "value",
          "nested": {
            "number": 21
          },
          "key2": "value"
        }
       "#;
        let mut got = ParseValueTest {
            key: String::new(),
            key2: String::new(),
            nested: Default::default(),
        };
        incremental_parse_parsable(input, &mut got)?;

        let want = ParseValueTest {
            key: String::from("value"),
            key2: String::from("value"),
            nested: Nested { number: 21 },
        };
        assert_eq!(want, got);
        Ok(())
    }
}
