use chumsky::span::SimpleSpan;
use std::ops::Deref;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn join(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn test_span() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn is_test(self) -> bool {
        self.start == 0 && self.end == 0
    }
}

impl From<SimpleSpan> for Span {
    fn from(value: SimpleSpan) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

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
        }
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T>(T, Span);

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
            && (self.span() == other.span()
                || self.span().is_test()
                || other.span().is_test())
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Spanned<T> {
    pub fn span(&self) -> Span {
        self.1
    }
}

pub trait WithSpan: Sized {
    fn with_span(self, span: impl Into<Span>) -> Spanned<Self> {
        Spanned(self, span.into())
    }

    fn test_span(self) -> Spanned<Self> {
        self.with_span(Span::test_span())
    }
}

impl<T> WithSpan for T {}
