use crate::tokens::*;

pub type OutputResult = Result<Output, ParseError>;

pub enum Output {
    Def {
        name: String,
        expr: Expr,
    },
    App {
        name: String,
        expr: Expr,
    },
    RevApp {
        name: String,
        expr: Expr,
    },
    Command {
        command: String,
        args: Vec<String>,
    },
}

impl std::fmt::Display for Output {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Output::Def { name, expr } => write!(f, "{} :: {}", name, expr),
            Output::App { name, expr } => write!(f, "{} :> {}", name, expr),
            Output::RevApp { name, expr } => write!(f, "{} :< {}", name, expr),
            Output::Command { command, args } => write!(f, ":{} {}", command, args.join(" ")),
        }
    }
}

pub type ExprResult = Result<Expr, ParseError>;

#[derive(Clone)]
pub enum Expr {
    Match(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Box<Expr>, Box<Expr>),
    Prefix(Box<Expr>, Box<Expr>),
    Postfix(Box<Expr>, Box<Expr>),
    Circumfix(Box<Expr>, Box<Expr>, Box<Expr>),
    Function(Box<Expr>, Box<Expr>),
    Vector(Vec<Expr>),
    Id(String, Context),
    LiteralId(String, Context),
    Op(String, Context),
    Int(usize, Context),
    Float(f64, Context),
    String(String, Context),
}

impl Expr {
    pub fn get_context(&self) -> Context {
        match self {
            Expr::Match(p, r) => Context::union(&p.get_context(), &r.get_context()),
            Expr::Binary(f, a, b) => a.get_context().union(&f.get_context()).union(&b.get_context()),
            Expr::Prefix(f, a) => f.get_context().union(&a.get_context()),
            Expr::Postfix(f, a) => a.get_context().union(&f.get_context()),
            Expr::Circumfix(f, g, a) => f.get_context().union(&a.get_context()).union(&g.get_context()),
            Expr::Function(f, a) => f.get_context().union(&a.get_context()),
            Expr::Vector(_) => unreachable!(),
            Expr::Id(_, c) => c.clone(),
            Expr::LiteralId(_, c) => c.clone(),
            Expr::Op(_, c) => c.clone(),
            Expr::Int(_, c) => c.clone(),
            Expr::Float(_, c) => c.clone(),
            Expr::String(_, c) => c.clone(),
        }
    }
}

impl Expr {
    pub fn success(self) -> ExprResult {
        Ok(self)
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Match(p1, r1), Expr::Match(p2, r2)) => p1 == p2 && r1 == r2,
            (Expr::Binary(f1, a1, b1), Expr::Binary(f2, a2, b2)) => f1 == f2 && a1 == a2 && b1 == b2,
            (Expr::Prefix(f1, a1), Expr::Prefix(f2, a2)) => f1 == f2 && a1 == a2,
            (Expr::Postfix(f1, a1), Expr::Postfix(f2, a2)) => f1 == f2 && a1 == a2,
            (Expr::Circumfix(f1, g1, a1), Expr::Circumfix(f2, g2, a2)) => f1 == f2 && g1 == g2 && a1 == a2,
            (Expr::Function(f1, a1), Expr::Function(f2, a2)) => f1 == f2 && a1 == a2,
            (Expr::Vector(v1), Expr::Vector(v2)) => v1.iter().zip(v2).all(|(a1, a2)| a1 == a2),
            (Expr::Id(id1, _), Expr::Id(id2, _)) => id1 == id2,
            (Expr::LiteralId(id1, _), Expr::LiteralId(id2, _)) => id1 == id2,
            (Expr::Id(id1, _), Expr::LiteralId(id2, _)) => id1 == id2,
            (Expr::LiteralId(id1, _), Expr::Id(id2, _)) => id1 == id2,
            (Expr::Op(op1, _), Expr::Op(op2, _)) => op1 == op2,
            (Expr::Int(int1, _), Expr::Int(int2, _)) => int1 == int2,
            (Expr::Float(float1, _), Expr::Float(float2, _)) => float1 == float2,
            (Expr::String(string1, _), Expr::String(string2, _)) => string1 == string2,
            _ => false,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.clone() {
            Expr::Match(pattern, result) => write!(f, "({} => {})", pattern, result),
            Expr::Binary(operator, left_expr, right_expr) => write!(f, "({} {} {})", left_expr, operator, right_expr),
            Expr::Prefix(operator, expr) => write!(f, "({} {})", operator, expr),
            Expr::Postfix(operator, expr) => write!(f, "({} {})", expr, operator),
            Expr::Circumfix(left_op, right_op, expr) => write!(f, "({} {} {})", left_op, expr, right_op),
            Expr::Function(operator, expr) => write!(f, "{}({})", operator, expr),
            Expr::Vector(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
            Expr::Id(id, _) => write!(f, "{}", id),
            Expr::LiteralId(lit_id, _) => write!(f, "\\{}", lit_id),
            Expr::Op(op, _) => write!(f, "{}", op),
            Expr::Int(int, _) => write!(f, "{}", int),
            Expr::Float(float, _) => write!(f, "{}", float),
            Expr::String(string, _) => write!(f, "\"{}\"", string),
        }
    }
}

#[derive(Clone)]
pub struct ParseError(pub ParseErrorType, pub Context);

#[derive(Clone)]
pub enum ParseErrorType {
    ExpressionExpected,
    MissingClosingDelimiter,
    UnrecognizedSymbol(String),
}

impl std::fmt::Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseErrorType::ExpressionExpected => write!(f, "Expected expression"),
            ParseErrorType::MissingClosingDelimiter => write!(f, "Missing a closing delimiter"),
            ParseErrorType::UnrecognizedSymbol(op) => write!(f, "Unrecognized operator {}", op),
        }
    }
}
