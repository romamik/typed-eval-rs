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
