use crate::nodes::{ParseError, ParseErrorType};
use crate::tokens::{Token, LexResult, Context, is_valid_symbol};
use crate::parser::Parser;

impl Parser {
    fn lex_word(&mut self) -> LexResult {
        let init_index = self.index;
        let mut word = String::new();
        loop {
            match self.chars[self.index..] {
                [x, ..] if x.is_alphabetic() || x.is_numeric() || x == '_' => {
                    word.push(x);
                    self.index += 1;
                },
                _ => break,
            }
        }
        let context = Context::new(init_index, self.index);
        match word.as_str() {
            "if" => Token::If(context).success(),
            _ => Token::Id(word, context).success(),
        }
    }

    fn lex_literal_id(&mut self) -> LexResult {
        let init_index = self.index;
        self.index += 1;
        let mut word = String::new();
        loop {
            match self.chars[self.index..] {
                [x, ..] if x.is_alphabetic() || x.is_numeric() || x == '_' => {
                    word.push(x);
                    self.index += 1;
                },
                _ => break,
            }
        }
        let context = Context::new(init_index, self.index);
        Token::LiteralId(word, context).success()
    }

    fn lex_string(&mut self) -> LexResult {
        let init_index = self.index;
        self.index += 1;
        let mut word = String::new();
        loop {
            match self.chars[self.index..] {
                ['"', ..] => break,
                [] | ['\n', ..] => return Err(ParseError(ParseErrorType::MissingClosingDelimiter, Context::new(self.index, self.index))),
                [x, ..] => {
                    word.push(x);
                    self.index += 1;
                },
            }
        }
        self.index += 1;
        let context = Context::new(init_index, self.index - 1);
        Token::String(word, context).success()
    }

    fn lex_number(&mut self) -> LexResult {
        let init_index = self.index;
        let mut word = String::new();
        loop {
            match self.chars[self.index..] {
                ['.', ..] if word.contains('.') => break,
                ['.', '0'..='9', ..]            => word.push('.'),
                ['.', ..]                       => break,
                ['_', ..]                       => {},
                [x @ '0'..='9', ..]             => word.push(x),
                _                               => break,
            }
            self.index += 1;
        }
        let context = Context::new(init_index, self.index - 1);
        if word.contains('.') {
            let float: f64 = word.parse().unwrap();
            Token::Float(float, context).success()
        } else {
            let int: usize = word.parse().unwrap();
            Token::Int(int, context).success()
        }
    }

    fn lex_symbol(&mut self) -> LexResult {
        let init_index = self.index;
        let mut word = String::new();
        while let [x, ..] = self.chars[self.index..] {
            if !is_valid_symbol(x) { break }
            word.push(x);
            self.index += 1;
        }
        let context = Context::new(init_index, self.index - 1);
        match word.as_str() {
            "=>" => Token::FatArrow(context).success(),
            "::" => Token::DoubleColon(context).success(),
            ":>" => Token::ColonRight(context).success(),
            ":<" => Token::ColonLeft(context).success(),
            _ if self.fixity.contains_op(&word) => Token::Op(word, context).success(),
            _ => Err(ParseError(ParseErrorType::UnrecognizedSymbol(word), context)),
        }
    }

    fn lex_strict_symbol(&mut self) -> LexResult {
        let context = Context::new(self.index, self.index + 1);
        let token = match self.chars[self.index] {
            '(' => Token::LeftParenthesis(context).success(),
            ')' => Token::RightParenthesis(context).success(),
            '[' => Token::LeftBracket(context).success(),
            ']' => Token::RightBracket(context).success(),
            '{' => Token::LeftBrace(context).success(),
            '}' => Token::RightBrace(context).success(),
            ',' => Token::Comma(context).success(),
            _ => unreachable!(),
        };
        self.index += 1;
        token
    }

    pub fn iter_token(&mut self) -> LexResult {
        self.token = match self.chars[self.index..] {
            [] => Ok(Token::EOF(Context::new(self.chars.len(), self.chars.len()))),
            ['"', ..] => self.lex_string(),
            ['\\', x, ..] if x.is_alphabetic() || x == '_' => self.lex_literal_id(),
            [x, ..] if x.is_alphabetic() || x == '_' => self.lex_word(),
            [x, ..] if x.is_numeric() => self.lex_number(),
            [x, ..] if x.is_whitespace() => {
                self.index += 1;
                return self.iter_token();
            }
            [x, ..] if "()[]{},".contains(x) => self.lex_strict_symbol(),
            [..] => self.lex_symbol(),
        };
        self.current_token()
    }

    pub fn current_token(&mut self) -> LexResult {
        self.token.clone()
    }

    pub fn peek_token(&mut self) -> LexResult {
        let index = self.index;
        let token = self.token.clone();
        let peeked_token = self.iter_token()?;
        self.index = index;
        self.token = token;
        Ok(peeked_token)
    }
}
