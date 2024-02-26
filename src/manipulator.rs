use std::collections::HashMap;

use crate::{nodes::Expr, tokens::Context};

pub type Definitions = HashMap<String, Expr>;

fn create_matchings(definitions: &mut Definitions, pattern: Expr, literal: Expr) -> bool {
    match (pattern, literal) {
        (Expr::Match(p1, r1), Expr::Match(p2, r2)) => create_matchings(definitions, *p1, *p2) && create_matchings(definitions, *r1, *r2),
        (Expr::Binary(f1, a1, b1), Expr::Binary(f2, a2, b2)) => create_matchings(definitions, *f1, *f2) && create_matchings(definitions, *a1, *a2) && create_matchings(definitions, *b1, *b2),
        (Expr::Prefix(f1, a1), Expr::Prefix(f2, a2)) => create_matchings(definitions, *f1, *f2) && create_matchings(definitions, *a1, *a2),
        (Expr::Postfix(f1, a1), Expr::Postfix(f2, a2)) => create_matchings(definitions, *f1, *f2) && create_matchings(definitions, *a1, *a2),
        (Expr::Circumfix(f1, g1, a1), Expr::Circumfix(f2, g2, a2)) => create_matchings(definitions, *f1, *f2) && create_matchings(definitions, *g1, *g2) && create_matchings(definitions, *a1, *a2),
        (Expr::Function(f1, a1), Expr::Function(f2, a2)) => create_matchings(definitions, *f1, *f2) && create_matchings(definitions, *a1, *a2),
        (Expr::Vector(v1), Expr::Vector(v2)) => v1.iter().zip(v2).all(|(a1, a2)| create_matchings(definitions, a1.clone(), a2)),
        (Expr::Int(int1, _), Expr::Int(int2, _)) => int1 == int2,
        (Expr::Float(int1, _), Expr::Float(int2, _)) => int1 == int2,
        (Expr::Int(int1, _), Expr::Float(float2, _)) => int1 as f64 == float2,
        (Expr::Float(float1, _), Expr::Int(int2, _)) => float1 == int2 as f64,
        (Expr::String(string1, _), Expr::String(string2, _)) => string1 == string2,
        (Expr::Op(op1, _), Expr::Op(op2, _)) => op1 == op2,
        (Expr::LiteralId(lit_id1, _), Expr::LiteralId(lit_id2, _)) => lit_id1 == lit_id2,
        (Expr::LiteralId(lit_id1, _), Expr::Id(id2, _)) => lit_id1 == id2,
        (Expr::Id(id, _), expr) => {
            if !definitions.contains_key(&id) {
                definitions.insert(id, expr);
                return true;
            }
            match (expr, definitions.get(&id)) {
                (a, Some(b)) if a == *b => true,
                _ => false,
            }
        },
        // PRELIMINARY SUBSTITUTIONS
        // a^b, x => x^1
        //     if pattern = a^b and literal = x, substitute x for x^1
        (Expr::Binary(op_expr, a1, b1), expr)
            if matches!(*op_expr.clone(), Expr::Op(op, _) if op == "^")
            => create_matchings(definitions, *a1, expr) && create_matchings(definitions, *b1, Expr::Int(1, Context::empty())),
        // a*b, x => 1*x
        //     if pattern = a*b and literal = x, substitute x for 1*x
        (Expr::Binary(op_expr, a1, b1), expr)
            if matches!(*op_expr.clone(), Expr::Op(op, _) if op == "*")
            => create_matchings(definitions, *a1, Expr::Int(1, Context::empty())) && create_matchings(definitions, *b1, expr),
        // -a, x => -(-x)
        //     if pattern = -a and literal = x, substitute x for -(-x)
        (Expr::Prefix(op_expr, a1), expr)
            if matches!(*op_expr.clone(), Expr::Op(op, _) if op == "-")
            => create_matchings(definitions, *a1, Expr::Prefix(op_expr, Box::new(expr))),
        // ~a, x => ~(~x)
        //     if pattern = ~a and literal = x, substitute x for ~(~x)
        // a+b, x-y => x+(-y)
        //     if pattern = a+b and literal = x-y, substitute x for x+(-y)
        // a-b, x+y => x-(-y)
        //     if pattern = a-b and literal = x+y, substitute x for x-(-y)
        _ => false,
    }
}

impl Expr {
    fn apply(&self, func: impl Fn(Expr) -> Expr) -> Self {
        match self {
            Expr::Match(pattern, result)     => Expr::Match(Box::new(func(*pattern.clone())), Box::new(func(*result.clone()))),
            Expr::Binary(op, lexpr, rexpr)   => Expr::Binary(Box::new(func(*op.clone())), Box::new(func(*lexpr.clone())), Box::new(func(*rexpr.clone()))),
            Expr::Prefix(op, expr)           => Expr::Prefix(Box::new(func(*op.clone())), Box::new(func(*expr.clone()))),
            Expr::Postfix(op, expr)          => Expr::Postfix(Box::new(func(*op.clone())), Box::new(func(*expr.clone()))),
            Expr::Circumfix(lop, rop, expr)  => Expr::Circumfix(Box::new(func(*lop.clone())), Box::new(func(*rop.clone())), Box::new(func(*expr.clone()))),
            Expr::Function(op, expr)         => Expr::Function(Box::new(func(*op.clone())), Box::new(func(*expr.clone()))),
            Expr::Vector(v)                  => Expr::Vector(v.iter().map(|a| func(a.clone())).collect()),
            _                                => self.clone()
        }
    }

    pub fn substitute(&self, pattern: &Expr, result: &Expr) -> Self {
        if *self == *pattern {
            result.clone()
        } else {
            self.apply(|a| a.substitute(pattern, result))
        }
    }

    pub fn substitute_definitions(&self, definitions: Definitions) -> Self {
        let mut expr = self.clone();
        for (name, defined_expr) in definitions {
            expr = expr.substitute(&Expr::Id(name, Context::empty()), &defined_expr);
        }
        expr
    }

    pub fn substitute_patterns(&self, pattern: &Expr, result: &Expr) -> Self {
        let mut definitions = HashMap::new();
        if create_matchings(&mut definitions, pattern.clone(), self.clone()) {
            result.substitute_definitions(definitions)
        } else {
            self.apply(|x| x.substitute_patterns(pattern, result))
        }
    }
}