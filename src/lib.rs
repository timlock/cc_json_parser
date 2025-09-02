use std::any::Any;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::num::ParseFloatError;
use std::str::{CharIndices, FromStr, ParseBoolError};

impl<'a> std::error::Error for Error<'a> {}

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
    UnexpectedToken(Token<'a>),
    UnexpectedChar(char),
    Incomplete,
    ParseFloatError(ParseFloatError),
    ParseBoolError(ParseBoolError),
    UnexpectedType(&'static str),
}

impl<'a> From<ParseFloatError> for Error<'a> {
    fn from(value: ParseFloatError) -> Self {
        Error::ParseFloatError(value)
    }
}

impl<'a> From<ParseBoolError> for Error<'a> {
    fn from(value: ParseBoolError) -> Self {
        Error::ParseBoolError(value)
    }
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedToken(token) => write!(
                f,
                "unexpected {:?} token {} at {}",
                token.token_type, token.slice, token.start
            ),
            Error::UnexpectedChar(character) => write!(f, "unexpected character {character}"),
            Error::Incomplete => write!(f, "incomplete state"),
            Error::ParseFloatError(err) => write!(f, "{}", err),
            Error::ParseBoolError(err) => write!(f, "{}", err),
            Error::UnexpectedType(want) => write!(f, "expected type {want}"),
        }
    }
}

pub trait Parser {
    fn parse_string(&mut self) -> Option<Result<String, Error>>;
    fn parse_f64(&mut self) -> Option<Result<f64, Error>>;

    fn parse_bool(&mut self) -> Option<Result<bool, Error>>;
}

pub trait Parsable {
    fn parse(&mut self, parser: &mut dyn Parser) -> Result<(), String>;
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
pub struct Token<'a> {
    token_type: TokenType,
    start: usize,
    len: usize,
    char: char,
    slice: &'a str,
}
impl<'a> Token<'a> {
    pub fn parse_string(&self) -> &'a str {
        if self.slice.is_empty() {
            return "";
        }
        &self.slice[1..self.slice.len() - 1]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    ObjectBegin,
    ObjectEnd,
    ArrayBegin,
    ArrayEnd,
    String,
    Literal,
    Colon,
    Comma,
}

struct Lexer<'a> {
    char_indices: Peekable<CharIndices<'a>>,
    content: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(content: &'a str) -> Self {
        Self {
            char_indices: content.char_indices().peekable(),
            content,
        }
    }

    fn next_token(&mut self) -> Option<Result<Token<'a>, Error<'a>>> {
        while let Some((index, character)) = self.char_indices.next() {
            if character.is_whitespace() {
                continue;
            }

            return Some(self.parse_token(index, character));
        }
        None
    }

    fn parse_token(&mut self, index: usize, character: char) -> Result<Token<'a>, Error<'a>> {
        let token = match character {
            ',' => {
                let len = character.len_utf8();
                Token {
                    token_type: TokenType::Comma,
                    start: index,
                    len,
                    char: character,
                    slice: &self.content[index..index + len],
                }
            }
            ':' => {
                let len = character.len_utf8();
                Token {
                    token_type: TokenType::Colon,
                    start: index,
                    len,
                    char: character,
                    slice: &self.content[index..index + len],
                }
            }
            '{' => {
                let len = character.len_utf8();
                Token {
                    token_type: TokenType::ObjectBegin,
                    start: index,
                    len,
                    char: character,
                    slice: &self.content[index..index + len],
                }
            }
            '}' => {
                let len = character.len_utf8();
                Token {
                    token_type: TokenType::ObjectEnd,
                    start: index,
                    len,
                    char: character,
                    slice: &self.content[index..index + len],
                }
            }
            '[' => {
                let len = character.len_utf8();
                Token {
                    token_type: TokenType::ArrayBegin,
                    start: index,
                    len,
                    char: character,
                    slice: &self.content[index..index + len],
                }
            }

            ']' => {
                let len = character.len_utf8();
                Token {
                    token_type: TokenType::ArrayEnd,
                    start: index,
                    len,
                    char: character,
                    slice: &self.content[index..index + len],
                }
            }
            '"' => match self.expect_string() {
                Ok(end) => {
                    let len = end + character.len_utf8() - index;
                    Token {
                        token_type: TokenType::String,
                        start: index,
                        len,
                        char: character,
                        slice: &self.content[index..index + len],
                    }
                }
                Err(err) => return Err(err),
            },
            _ if character.is_alphanumeric()
                || character == '+'
                || character == '-'
                || character == '.' =>
            {
                let len = self.expect_literal() + character.len_utf8();
                Token {
                    token_type: TokenType::Literal,
                    start: index,
                    len,
                    char: character,
                    slice: &self.content[index..index + len],
                }
            }

            _ => return Err(Error::UnexpectedChar(character)),
        };
        Ok(token)
    }

    fn last_consumed_char_is_escaped(&mut self) -> bool {
        let index = self
            .char_indices
            .peek()
            .unwrap_or(&self.content.char_indices().last().expect(
            "Should not check if character is escaped when the lexer works on zero length content",
        )).0;
        let iter = &mut self.content[..index].char_indices().rev().skip(1);

        let mut is_escaped = false;
        while let Some((_, '\\')) = iter.next() {
            is_escaped = !is_escaped;
        }

        is_escaped
    }

    fn expect_string(&mut self) -> Result<usize, Error<'a>> {
        loop {
            let (index, char) = self.char_indices.next().ok_or(Error::Incomplete)?;
            if char != '"' {
                continue;
            }
            if !self.last_consumed_char_is_escaped() {
                return Ok(index);
            }
        }
    }

    fn expect_literal(&mut self) -> usize {
        let mut count = 0;
        loop {
            let (index, character) = match self.char_indices.peek() {
                Some((index, character)) => (index, character),
                None => break,
            };

            if character.is_alphanumeric()
                || *character == '+'
                || *character == '-'
                || *character == '.'
            {
                count += 1;
                self.char_indices.next();
            } else {
                break;
            }
        }
        count
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub fn parse(content: &str) -> Result<JsonValue, Error> {
    let mut parser = JSONParser::new(content);
    parser.next()
}

pub fn incremental_parse_parsable<T>(src: &str, dst: &mut T) -> Result<(), String>
where
    T: Parsable,
{
    let mut parser = IncrementalParser::new(src);
    dst.parse(&mut parser)
}

impl<'a> Parser for IncrementalParser<'a> {
    fn parse_string(&mut self) -> Option<Result<String, Error>> {
        match self.next_value()? {
            Ok(JsonValue::String(string)) => Some(Ok(string)),
            Ok(other) => Some(Err(Error::UnexpectedType("string"))),
            Err(err) => Some(Err(err)),
        }
    }

    fn parse_f64(&mut self) -> Option<Result<f64, Error>> {
        match self.next_value()? {
            Ok(JsonValue::Number(number)) => Some(Ok(number)),
            Ok(other) => Some(Err(Error::UnexpectedType("number"))),
            Err(err) => Some(Err(err)),
        }
    }

    fn parse_bool(&mut self) -> Option<Result<bool, Error>> {
        match self.next_value()? {
            Ok(JsonValue::Bool(bool)) => Some(Ok(bool)),
            Ok(other) => Some(Err(Error::UnexpectedType("bool"))),
            Err(err) => Some(Err(err)),
        }
    }
}

#[derive(Clone, Copy)]
struct ObjectValidator {
    state: ObjectValidatorState,
}

impl<'a> ObjectValidator {
    fn apply(&mut self, token: Token<'a>) -> Result<(), Error<'a>> {
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

impl<'a> ObjectValidatorState {
    fn apply(self, token: Token<'a>) -> Result<ObjectValidatorState, Error<'a>> {
        match self {
            ObjectValidatorState::Begin => match token.token_type {
                TokenType::ObjectEnd => Ok(ObjectValidatorState::End),
                TokenType::String => Ok(ObjectValidatorState::Key),
                _ => Err(Error::UnexpectedToken(token)),
            },
            ObjectValidatorState::Key => match token.token_type {
                TokenType::Colon => Ok(ObjectValidatorState::Colon),
                _ => Err(Error::UnexpectedToken(token)),
            },
            ObjectValidatorState::Colon => match token.token_type {
                TokenType::ObjectBegin
                | TokenType::ArrayBegin
                | TokenType::String
                | TokenType::Literal => Ok(ObjectValidatorState::Value),
                _ => Err(Error::UnexpectedToken(token)),
            },
            ObjectValidatorState::Value => match token.token_type {
                TokenType::Comma => Ok(ObjectValidatorState::Comma),
                TokenType::ObjectEnd => Ok(ObjectValidatorState::End),
                _ => Err(Error::UnexpectedToken(token)),
            },
            ObjectValidatorState::Comma => match token.token_type {
                TokenType::String => Ok(ObjectValidatorState::Key),
                _ => Err(Error::UnexpectedToken(token)),
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

impl<'a> ArrayValidator {
    fn apply(&mut self, token: Token<'a>) -> Result<(), Error<'a>> {
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

impl<'a> ArrayValidatorState {
    fn apply(self, token: Token<'a>) -> Result<ArrayValidatorState, Error<'a>> {
        match self {
            ArrayValidatorState::Begin => match token.token_type {
                TokenType::ArrayEnd => Ok(ArrayValidatorState::End),
                TokenType::ObjectBegin
                | TokenType::ArrayBegin
                | TokenType::String
                | TokenType::Literal => Ok(ArrayValidatorState::Value),
                _ => Err(Error::UnexpectedToken(token)),
            },
            ArrayValidatorState::Value => match token.token_type {
                TokenType::Comma => Ok(ArrayValidatorState::Comma),
                TokenType::ArrayEnd => Ok(ArrayValidatorState::End),
                _ => Err(Error::UnexpectedToken(token)),
            },
            ArrayValidatorState::Comma => match token.token_type {
                TokenType::ObjectBegin
                | TokenType::ArrayBegin
                | TokenType::String
                | TokenType::Literal => Ok(ArrayValidatorState::Value),
                _ => Err(Error::UnexpectedToken(token)),
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
    inner: JSONParser<'a>,
    validator_stack: Vec<Validator>,
}

impl<'a> IncrementalParser<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            inner: JSONParser::new(content),
            validator_stack: Vec::new(),
        }
    }

    fn next_value(&mut self) -> Option<Result<JsonValue, Error<'a>>> {
        if self.validator_stack.is_empty() {
            let next_token = self.inner.lexer.next()?;
            let next_token = match next_token {
                Ok(token) => token,
                Err(err) => return Some(Err(err)),
            };

            let validator = match next_token.token_type {
                TokenType::ObjectBegin => Validator::Object(ObjectValidator::default()),
                TokenType::ArrayBegin => Validator::Array(ArrayValidator::default()),
                TokenType::String => return Some(self.inner.expect_string().map(JsonValue::from)),
                TokenType::Literal => return Some(self.inner.parse_literal(next_token)),
                _ => {
                    return Some(Err(Error::UnexpectedToken(next_token)));
                }
            };

            self.validator_stack.push(validator);
        }

        loop {
            let token = match self.next_container_value() {
                Ok(token) => token,
                Err(err) => return Some(Err(err)),
            };

            match token.token_type {
                TokenType::ObjectBegin => self
                    .validator_stack
                    .push(Validator::Object(ObjectValidator::default())),
                TokenType::ArrayBegin => self
                    .validator_stack
                    .push(Validator::Array(ArrayValidator::default())),
                TokenType::String => return Some(self.inner.expect_string().map(JsonValue::from)),
                TokenType::Literal => return Some(self.inner.parse_literal(token)),
                TokenType::ObjectEnd => {
                    self.validator_stack.pop();
                    return None;
                }
                TokenType::ArrayEnd => {
                    self.validator_stack.pop();
                    return None;
                }
                _ => panic!("next_object_value should not return {token:?}"),
            }
        }
    }

    fn next_container_value(&mut self) -> Result<Token<'a>, Error<'a>> {
        loop {
            let token = self.inner.lexer.next().ok_or(Error::Incomplete)??;

            let validator = self.validator_stack.last_mut().ok_or(Error::Incomplete)?;

            match validator {
                Validator::Object(validator_state) => {
                    validator_state.apply(token)?;
                    match token.token_type {
                        TokenType::ObjectBegin
                        | TokenType::ObjectEnd
                        | TokenType::ArrayBegin
                        | TokenType::String
                        | TokenType::Literal => return Ok(token),
                        TokenType::Colon | TokenType::Comma => {
                            // do nothing, we are only interested in values
                        }
                        _ => panic!(
                            "Should not be possible that an object validator validates a {token:?}"
                        ),
                    }
                }
                Validator::Array(validator_state) => {
                    validator_state.apply(token)?;
                    match token.token_type {
                        TokenType::ObjectBegin
                        | TokenType::ArrayEnd
                        | TokenType::ArrayBegin
                        | TokenType::String
                        | TokenType::Literal => return Ok(token),
                        TokenType::Comma => {
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

pub struct JSONParser<'a> {
    lexer: Peekable<Lexer<'a>>,
    content: &'a str,
}

impl<'a> JSONParser<'a> {
    pub fn new(content: &str) -> JSONParser {
        JSONParser {
            lexer: Lexer::new(content).peekable(),
            content,
        }
    }

    pub fn next(&mut self) -> Result<JsonValue, Error<'a>> {
        self.expect_value()
    }

    pub fn parse_object(
        &mut self,
        want: &mut BTreeMap<String, JsonValue>,
    ) -> Result<(), Error<'a>> {
        self.expect_token(TokenType::ObjectBegin)?;
        let mut got = self.expect_object()?;

        for (key, value) in want.iter_mut() {
            if let Some(got_value) = got.remove(key) {
                *value = got_value
            }
        }

        Ok(())
    }

    fn expect_value(&mut self) -> Result<JsonValue, Error<'a>> {
        let token = self.lexer.next().ok_or(Error::Incomplete)??;
        match token.token_type {
            TokenType::ObjectBegin => self.expect_object().map(JsonValue::from),
            TokenType::ArrayBegin => self.expect_array().map(JsonValue::from),
            TokenType::String => {
                let string = String::from(token.parse_string());
                Ok(JsonValue::String(string))
            }
            TokenType::Literal => self.parse_literal(token),
            _ => Err(Error::UnexpectedToken(token)),
        }
    }

    fn parse_literal(&self, token: Token<'a>) -> Result<JsonValue, Error<'a>> {
        match token.char {
            '-' | '0'..='9' => {
                let number = token.slice.parse()?;
                Ok(JsonValue::Number(number))
            }
            't' | 'f' => {
                let boolean = bool::from_str(token.slice)?;
                Ok(JsonValue::Bool(boolean))
            }
            'n' => match token.slice.to_lowercase().as_str() {
                "null" => Ok(JsonValue::Null),
                _ => Err(Error::UnexpectedToken(token)),
            },
            _ => Err(Error::UnexpectedToken(token)),
        }
    }

    fn expect_object(&mut self) -> Result<BTreeMap<String, JsonValue>, Error<'a>> {
        let mut object = BTreeMap::new();

        if self.next_token_is(TokenType::ObjectEnd) {
            self.expect_token(TokenType::ObjectEnd)?;
            return Ok(object);
        }

        loop {
            let token = self.expect_token(TokenType::String)?;
            let key = String::from(token.parse_string());

            self.expect_token(TokenType::Colon)?;
            let value = self.expect_value()?;
            object.insert(key, value);

            if !self.next_token_is(TokenType::Comma) {
                break;
            }
            self.expect_token(TokenType::Comma)?;
        }

        self.expect_token(TokenType::ObjectEnd)?;

        Ok(object)
    }

    fn expect_array(&mut self) -> Result<Vec<JsonValue>, Error<'a>> {
        let mut array = Vec::new();

        if self.next_token_is(TokenType::ArrayEnd) {
            self.expect_token(TokenType::ArrayEnd)?;
            return Ok(array);
        }

        loop {
            let item = self.expect_value()?;
            array.push(item);

            if !self.next_token_is(TokenType::Comma) {
                break;
            }
            self.expect_token(TokenType::Comma)?;
        }

        self.expect_token(TokenType::ArrayEnd)?;

        Ok(array)
    }

    fn expect_string(&mut self) -> Result<String, Error<'a>> {
        let token = self.lexer.next().ok_or(Error::Incomplete)??;
        match token.token_type {
            TokenType::String => {}
            _ => return Err(Error::UnexpectedToken(token)),
        };

        Ok(String::from(
            &self.content[token.start..(token.start + token.len)],
        ))
    }

    fn expect_token(&mut self, want: TokenType) -> Result<Token<'a>, Error<'a>> {
        match self.lexer.next() {
            Some(Ok(token)) if token.token_type == want => Ok(token),
            Some(Ok(token)) => Err(Error::UnexpectedToken(token)),
            Some(Err(err)) => Err(err),
            None => Err(Error::Incomplete),
        }
    }

    fn next_token_is(&mut self, want: TokenType) -> bool {
        match self.lexer.peek() {
            Some(Ok(token)) if token.token_type == want => true,
            _ => false,
        }
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

    impl Parsable for Nested {
        fn parse(&mut self, parser: &mut dyn Parser) -> Result<(), String> {
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
        fn parse(&mut self, parser: &mut dyn Parser) -> Result<(), String> {
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
        fn parse(&mut self, parser: &mut dyn Parser) -> Result<(), String> {
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
