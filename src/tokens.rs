use crate::nodes::ParseError;

pub type LexResult = Result<Token, ParseError>;

#[derive(Clone)]
pub struct Context {
    pub init: usize,
    pub tail: usize,
}

impl Context {
    pub fn empty() -> Self {
        Context {
            init: 0,
            tail: 0,
        }
    }

    pub fn new(i: usize, f: usize) -> Self {
        Context {
            init: i,
            tail: f,
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        Context {
            init: std::cmp::min(self.init, other.init),
            tail: std::cmp::max(self.tail, other.tail),
        }
    }
}

#[derive(Clone)]
pub enum Token {
    Id(String, Context),
    Op(String, Context),
    LiteralId(String, Context),
    Int(usize, Context),
    Float(f64, Context),
    String(String, Context),
    LeftParenthesis(Context),
    RightParenthesis(Context),
    LeftBracket(Context),
    RightBracket(Context),
    LeftBrace(Context),
    RightBrace(Context),
    Comma(Context),
    FatArrow(Context),
    DoubleColon(Context),
    ColonRight(Context),
    ColonLeft(Context),
    If(Context),
    BOF(Context),
    EOF(Context),
}

impl Token {
    pub fn success(self) -> LexResult {
        Ok(self)
    }

    pub fn get_context(self) -> Context {
        match self {
            Token::Id(_, context) => context,
            Token::Op(_, context) => context,
            Token::LiteralId(_, context) => context,
            Token::Int(_, context) => context,
            Token::Float(_, context) => context,
            Token::String(_, context) => context,
            Token::LeftParenthesis(context) => context,
            Token::RightParenthesis(context) => context,
            Token::LeftBracket(context) => context,
            Token::RightBracket(context) => context,
            Token::LeftBrace(context) => context,
            Token::RightBrace(context) => context,
            Token::Comma(context) => context,
            Token::FatArrow(context) => context,
            Token::DoubleColon(context) => context,
            Token::ColonRight(context) => context,
            Token::ColonLeft(context) => context,
            Token::If(context) => context,
            Token::BOF(context) => context,
            Token::EOF(context) => context,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.clone() {
            Token::Id(s, _)            => write!(f, "[id {}]", s),
            Token::Op(s, _)            => write!(f, "[op {}]", s),
            Token::LiteralId(s, _)     => write!(f, "[lit-id {}]", s),
            Token::Int(n, _)           => write!(f, "[int {}]", n),
            Token::Float(n, _)         => write!(f, "[float {}]", n),
            Token::String(s, _)        => write!(f, "[str {}]", s),
            Token::LeftParenthesis(_)  => write!(f, "[kw '(']"),
            Token::RightParenthesis(_) => write!(f, "[kw ')']"),
            Token::LeftBracket(_)      => write!(f, "[kw '[']"),
            Token::RightBracket(_)     => write!(f, "[kw ']']"),
            Token::LeftBrace(_)        => write!(f, "[kw '{{']"),
            Token::RightBrace(_)       => write!(f, "[kw '}}']"),
            Token::Comma(_)            => write!(f, "[kw ',']"),
            Token::FatArrow(_)         => write!(f, "[kw '=>']"),
            Token::DoubleColon(_)      => write!(f, "[kw '::']"),
            Token::ColonRight(_)       => write!(f, "[kw ':>']"),
            Token::ColonLeft(_)        => write!(f, "[kw ':<']"),
            Token::If(_)               => write!(f, "[kw if]"),
            Token::BOF(_)              => write!(f, "[BOF]"),
            Token::EOF(_)              => write!(f, "[EOF]"),
        }
    }
}

pub fn is_valid_symbol(character: char) -> bool {
    "!@#$%^&*-+=\\/?<>.\':;~`|".contains(character)
}