use crate::lexer::Token;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::num::ParseFloatError;
use std::str::{ParseBoolError};

pub mod lexer;
pub mod parser;

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
                "unexpected {:?} token '{}' at {}",
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


#[derive(Debug, PartialEq)]
pub enum JsonValue {
    String(String),
    Number(f64),
    Object(BTreeMap<String, JsonValue>),
    Array(Vec<JsonValue>),
    Bool(bool),
    Null,
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