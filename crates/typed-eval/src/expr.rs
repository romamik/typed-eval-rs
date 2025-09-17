use crate::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(Spanned<i64>),
    Float(Spanned<f64>),
    String(Spanned<String>),
    Var(Spanned<String>),
    UnOp(Spanned<UnOp>, Box<Expr>),
    BinOp(Spanned<BinOp>, Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, Spanned<String>),
    FuncCall(Box<Expr>, Spanned<Vec<Expr>>),
    InvalidLiteral(Spanned<String>),
    ParseError,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Mul,
    Sub,
    Div,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Plus,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(val) => val.span(),
            Expr::Float(val) => val.span(),
            Expr::String(val) => val.span(),
            Expr::Var(s) => s.span(),
            Expr::UnOp(op, rhs) => op.span().join(rhs.span()),
            Expr::BinOp(op, lhs, rhs) => {
                op.span().join(lhs.span()).join(rhs.span())
            }
            Expr::FieldAccess(obj, field) => obj.span().join(field.span()),
            Expr::FuncCall(func, args) => func.span().join(args.span()),
            Expr::InvalidLiteral(err) => err.span(),
            Expr::ParseError => Span::test_span(),
        }
    }
}
