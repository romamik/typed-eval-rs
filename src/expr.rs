#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Var(String),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    FieldAccess(Box<Expr>, String),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Mul,
    Sub,
    Div,
}

impl BinOp {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(BinOp::Add),
            '*' => Some(BinOp::Mul),
            '-' => Some(BinOp::Sub),
            '/' => Some(BinOp::Div),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Plus,
}

impl UnOp {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(UnOp::Plus),
            '-' => Some(UnOp::Neg),
            _ => None,
        }
    }
}
