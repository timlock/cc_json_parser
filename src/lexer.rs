use crate::Error;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub start: usize,
    pub len: usize,
    pub char: char,
    pub slice: &'a str,
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

pub struct Lexer<'a> {
    char_indices: Peekable<CharIndices<'a>>,
    content: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a str) -> Self {
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
