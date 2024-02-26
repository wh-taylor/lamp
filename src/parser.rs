use crate::tokens::{Token, Context};
use crate::nodes::{ExprResult, OutputResult, ParseError, ParseErrorType, Expr, Output};
use crate::tokens::LexResult;

#[derive(Clone)]
pub struct Fixity {
    pub layers: Vec<FixityLayer>,
    pub circumfix: Vec<(String, String)>,
}

impl Fixity {
    pub fn new() -> Self {
        Fixity {
            layers: vec![FixityLayer::new()],
            circumfix: Vec::new(),
        }
    }

    pub fn contains_op(&self, op: &String) -> bool {
        for layer in self.layers.iter() {
            if layer.as_vec().contains(&op) {
                return true;
            }
        }
        self.circumfix.iter().any(|(op1, op2)| op == op1 || op == op2)
    }
}

#[derive(Clone)]
pub struct FixityLayer {
    pub infix: Vec<String>,
    pub infixl: Vec<String>,
    pub infixr: Vec<String>,
    pub prefix: Vec<String>,
    pub postfix: Vec<String>,
}

impl FixityLayer {
    pub fn new() -> Self {
        FixityLayer {
            infix: Vec::new(),
            infixl: Vec::new(),
            infixr: Vec::new(),
            prefix: Vec::new(),
            postfix: Vec::new(),
        }
    }

    pub fn add_operator(&mut self, fixity_type: FixityType, operator: String) {
        match fixity_type {
            FixityType::Infix => self.infix.push(operator),
            FixityType::InfixL => self.infixl.push(operator),
            FixityType::InfixR => self.infixr.push(operator),
            FixityType::Prefix => self.prefix.push(operator),
            FixityType::Postfix => self.postfix.push(operator),
        }
    }

    pub fn as_vec(&self) -> Vec<String> {
        let mut vec = Vec::new();
        vec.extend(self.infix.clone());
        vec.extend(self.infixl.clone());
        vec.extend(self.infixr.clone());
        vec.extend(self.prefix.clone());
        vec.extend(self.postfix.clone());
        vec
    }
}

pub enum FixityType {
    Infix,
    InfixR,
    InfixL,
    Prefix,
    Postfix,
}

impl std::fmt::Display for FixityType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            FixityType::Infix => write!(f, "infix"),
            FixityType::InfixL => write!(f, "infixl"),
            FixityType::InfixR => write!(f, "infixr"),
            FixityType::Prefix => write!(f, "prefix"),
            FixityType::Postfix => write!(f, "postfix"),
        }
    }
}

#[derive(Clone)]
pub struct Parser {
    pub chars: Vec<char>,
    pub index: usize,
    pub token: LexResult,
    pub fixity: Fixity,
    pub layer: usize,
}

impl Parser {
    pub fn new(code: String, fixity: Fixity) -> Parser {
        let chars: Vec<char> = code.chars().collect();
        Parser {
            chars: chars,
            index: 0,
            token: Ok(Token::BOF(Context::new(0, 0))),
            fixity,
            layer: 0,
        }
    }

    pub fn parse(&mut self) -> OutputResult {
        if self.chars.starts_with(&[':']) && !self.chars.starts_with(&[':', '>']) && !self.chars.starts_with(&[':', '<']) {
            self.parse_command()
        } else {
            self.parse_definition()
        }
    }

    fn layer_is_valid(&mut self) -> bool {
        self.layer < self.fixity.layers.len()
    }

    pub fn layer(&mut self) -> &FixityLayer {
        self.fixity.layers.get(self.layer).unwrap()
    }

    fn parse_command(&mut self) -> OutputResult {
        let mut args = Vec::new();

        let text: String = self.chars.iter().collect();
        let split_text: Vec<_> = text.split("\"").collect();

        for (s, i) in split_text.iter().zip(0..) {
            if (i == 0 || i == split_text.len() - 1) && *s == "" { continue; }
            if i % 2 == 1 {
                args.push(s.to_string());
            } else {
                for sub in s.trim().split_whitespace() {
                    args.push(sub.to_string());
                }
            }
        }

        let command = args.remove(0);
        Ok(Output::Command { command, args })
    }

    fn parse_definition(&mut self) -> OutputResult {
        match self.iter_token()? {
            Token::Id(name, _) => {
                match self.peek_token()? {
                    Token::DoubleColon(_) => {
                        self.iter_token()?;
                        self.iter_token()?;
                        let expr = self.parse_expression()?;
                        Ok(Output::Def { name, expr })
                    },
                    Token::ColonRight(_) => {
                        self.iter_token()?;
                        self.iter_token()?;
                        let expr = self.parse_expression()?;
                        Ok(Output::App { name, expr })
                    },
                    Token::ColonLeft(_) => {
                        self.iter_token()?;
                        self.iter_token()?;
                        let expr = self.parse_expression()?;
                        Ok(Output::RevApp { name, expr })
                    },
                    _ => {
                        let expr = self.parse_expression()?;
                        Ok(Output::Def { name: "ans".to_string(), expr })
                    },
                }
            },
            Token::ColonRight(_) => {
                self.iter_token()?;
                let expr = self.parse_expression()?;
                Ok(Output::App { name: "ans".to_string(), expr })
            },
            Token::ColonLeft(_) => {
                self.iter_token()?;
                let expr = self.parse_expression()?;
                Ok(Output::RevApp { name: "ans".to_string(), expr })
            },
            _ => {
                let expr = self.parse_expression()?;
                Ok(Output::Def { name: "ans".to_string(), expr })
            }
        }
    }

    fn parse_expression(&mut self) -> ExprResult {
        if self.layer_is_valid() {
            self.parse_match()
        } else {
            self.parse_function()
        }
    }

    fn parse_match(&mut self) -> ExprResult {
        let mut expr = self.parse_infix()?;
        if self.layer != 0 { return Ok(expr); }
        match self.current_token()? {
            Token::FatArrow(_) => {
                self.iter_token()?;
                let left_expr = Box::new(expr);
                let right_expr = Box::new(self.parse_infix()?);
                expr = Expr::Match(left_expr, right_expr);
            }
            _ => (),
        }
        Ok(expr)
    }

    fn parse_infix(&mut self) -> ExprResult {
        let mut expr = self.parse_infixl()?;
        match self.current_token()? {
            Token::Op(op, context) if self.layer().infix.contains(&op) => {
                self.iter_token()?;
                let left_expr = Box::new(expr);
                let right_expr = Box::new(self.parse_infixl()?);
                expr = Expr::Binary(Box::new(Expr::Op(op, context)), left_expr, right_expr);
            }
            _ => (),
        }
        Ok(expr)
    }

    fn parse_infixl(&mut self) -> ExprResult {
        let mut expr = self.parse_infixr()?;
        loop {
            match self.current_token()? {
                Token::Op(op, context) if self.layer().infixl.contains(&op) => {
                    self.iter_token()?;
                    let left_expr = Box::new(expr);
                    let right_expr = Box::new(self.parse_infixr()?);
                    expr = Expr::Binary(Box::new(Expr::Op(op, context)), left_expr, right_expr);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_infixr(&mut self) -> ExprResult {
        let expr = self.parse_prefix()?;
        match self.current_token()? {
            Token::Op(op, context) if self.layer().infixr.contains(&op) => {
                self.iter_token()?;
                let left_expr = Box::new(expr);
                let right_expr = Box::new(self.parse_infixr()?);
                Ok(Expr::Binary(Box::new(Expr::Op(op, context)), left_expr, right_expr))
            },
            _ => Ok(expr),
        }
    }

    fn parse_prefix(&mut self) -> ExprResult {
        match self.current_token()? {
            Token::Op(op, context) if self.layer().prefix.contains(&op) => {
                self.iter_token()?;
                let expr = self.parse_prefix()?;
                Ok(Expr::Prefix(Box::new(Expr::Op(op, context)), Box::new(expr)))
            },
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> ExprResult {
        self.layer += 1;
        let mut expr = self.parse_expression()?;
        self.layer -= 1;
        loop {
            match self.current_token()? {
                Token::Op(op, context) if self.layer().postfix.contains(&op) => {
                    expr = Expr::Postfix(Box::new(Expr::Op(op, context)), Box::new(expr))
                },
                _ => break,
            }
            self.iter_token()?;
        }
        Ok(expr)
    }

    fn parse_function(&mut self) -> ExprResult {
        let mut expr = self.parse_atom()?;
        loop {
            match self.current_token()? {
                Token::LeftParenthesis(_) | Token::Id(_, _) | Token::Int(_, _) | Token::Float(_, _) | Token::String(_, _) => {
                    let operator = Box::new(expr);
                    let input = Box::new(self.parse_atom()?);
                    expr = Expr::Function(operator, input);
                }
                _ => return Ok(expr),
            }
        }
    }

    fn parse_atom(&mut self) -> ExprResult {
        let expr: ExprResult = match self.current_token()? {
            Token::String(string, context)       => Expr::String(string, context).success(),
            Token::Int(int, context)          => Expr::Int(int, context).success(),
            Token::Float(float, context)      => Expr::Float(float, context).success(),
            Token::Id(id, context)            => Expr::Id(id, context).success(),
            Token::LiteralId(lit_id, context) => Expr::LiteralId(lit_id, context).success(),
            Token::LeftParenthesis(_)         => self.parse_parentheses(),
            Token::LeftBrace(_)               => self.parse_set(),
            Token::Op(op, _) if self.fixity.circumfix.iter().any(|x| x.0 == op) => self.parse_circumfix(),
            t => Err(ParseError(ParseErrorType::ExpressionExpected, t.get_context()))?,
        };
        self.iter_token()?;
        expr
    }

    fn parse_circumfix(&mut self) -> ExprResult {
        let Token::Op(left_op_str, left_op_context) = self.current_token()? else { unreachable!() };
        self.iter_token()?;

        let layer = self.layer;
        self.layer = 0;
        let expr = self.parse_expression()?;
        self.layer = layer;

        match self.current_token()? {
            Token::Op(right_op_str, right_op_context)
                if self.fixity.circumfix.iter().filter(|x| x.0 == left_op_str).collect::<Vec<_>>().iter().any(|x| x.1 == right_op_str) => {
                Ok(Expr::Circumfix(Box::new(Expr::Op(left_op_str, left_op_context)), Box::new(Expr::Op(right_op_str, right_op_context)), Box::new(expr)))
            },
            t => Err(ParseError(ParseErrorType::MissingClosingDelimiter, t.get_context()))
        }
    }

    fn parse_parentheses(&mut self) -> ExprResult {
        if let Token::RightParenthesis(_) = self.iter_token()? {
            return Ok(Expr::Vector(Vec::new()));
        }

        let layer = self.layer;
        self.layer = 0;
        let expr = self.parse_expression()?;
        self.layer = layer;

        match self.current_token()? {
            Token::Comma(_) => {
                let mut exprs = Vec::new();
                exprs.push(expr);
                loop {
                    if let Token::RightParenthesis(_) = self.iter_token()? {
                        break;
                    } else {
                        let layer = self.layer;
                        self.layer = 0;
                        let this_expr = self.parse_expression()?;
                        self.layer = layer;
                        exprs.push(this_expr);
                        match self.current_token()? {
                            Token::RightParenthesis(_) => break,
                            Token::Comma(_) => continue,
                            _ => return Err(ParseError(ParseErrorType::ExpressionExpected, self.current_token()?.get_context())),
                        }
                    }
                }
                Ok(Expr::Vector(exprs))
            },
            Token::RightParenthesis(_) => {
                Ok(expr)
            },
            t => Err(ParseError(ParseErrorType::MissingClosingDelimiter, t.get_context()))
        }
    }

    fn parse_set(&mut self) -> ExprResult {
        if let Token::RightParenthesis(_) = self.iter_token()? {
            return Ok(Expr::Vector(Vec::new()));
        }

        let layer = self.layer;
        self.layer = 0;
        let expr = self.parse_expression()?;
        self.layer = layer;

        match self.current_token()? {
            Token::Comma(_) => {
                let mut exprs = Vec::new();
                exprs.push(expr);
                loop {
                    if let Token::RightParenthesis(_) = self.iter_token()? {
                        break;
                    } else {
                        let layer = self.layer;
                        self.layer = 0;
                        let this_expr = self.parse_expression()?;
                        self.layer = layer;
                        exprs.push(this_expr);
                        match self.current_token()? {
                            Token::RightParenthesis(_) => break,
                            Token::Comma(_) => continue,
                            _ => return Err(ParseError(ParseErrorType::ExpressionExpected, self.current_token()?.get_context())),
                        }
                    }
                }
                Ok(Expr::Vector(exprs))
            },
            Token::RightParenthesis(_) => {
                Ok(expr)
            },
            t => Err(ParseError(ParseErrorType::MissingClosingDelimiter, t.get_context()))
        }
    }
}
