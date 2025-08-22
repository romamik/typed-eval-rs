#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    Var(String),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, String),
    FuncCall(Box<Expr>, Vec<Expr>),
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

macro_rules! from_char {
    ($type:ty { $($char:literal => $variant:ident),* } ) => {
        impl $type {
            pub fn from_char(c: char) -> Option<Self> {
                match c {
                    $( $char => Some(<$type>::$variant), )*
                    _ => None,
                }
            }
        }
    };
}
from_char!(UnOp { '+'=>Plus, '-'=>Neg });
from_char!(BinOp { '+'=>Add, '-'=>Sub, '*'=>Mul, '/'=>Div });
