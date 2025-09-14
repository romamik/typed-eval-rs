use thiserror::Error;

use crate::{BinOp, TypeInfo, UnOp};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("Wrong number of arguments: expected {expected}, got {got}")]
    ArgCountMismatch { expected: usize, got: usize },

    #[error("Cannot cast from type `{from}` to `{to}`")]
    CantCast { from: TypeInfo, to: TypeInfo },

    #[error("Cannot cast expressions of type `{0}` and `{1}` to same type")]
    CantCastSameType(TypeInfo, TypeInfo),

    #[error("No unary operator {op:?} for type `{ty}`")]
    UnknownUnaryOp { op: UnOp, ty: TypeInfo },

    #[error("No binary operator {op:?} for type `{ty}`")]
    UnknownBinaryOp { op: BinOp, ty: TypeInfo },

    #[error("Field `{field}` not found for type `{ty}`")]
    FieldNotFound { ty: TypeInfo, field: String },

    #[error("Method `{method}` not found for type `{ty}`")]
    MethodNotFound { ty: TypeInfo, method: String },

    #[error("Unsupported function call")]
    UnsupportedFunctionCall,

    #[error("Cast from `{from}` to `{to}` already registered")]
    DuplicateCast { from: TypeInfo, to: TypeInfo },

    #[error("Unary operation {op:?} on type `{ty}` already registered")]
    DuplicateUnOp { op: UnOp, ty: TypeInfo },

    #[error("Binary operation {op:?} on type `{ty}` already registered")]
    DuplicateBinOp { op: BinOp, ty: TypeInfo },

    #[error("Field access `{field}` on type `{ty}` already registered")]
    DuplicateField { ty: TypeInfo, field: &'static str },

    #[error("Method `{method}` on type `{ty}` already registered")]
    DuplicateMethod { ty: TypeInfo, method: &'static str },

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error(
        "Internal compiler error: Wrong number of arguments, expected {expected}, got {got}"
    )]
    InternalArgCountMismatch { expected: usize, got: usize },

    #[error(
        "Internal compiler error: DynFn downcast failed, expected {expected_arg}->{expected_ret}, got {got_arg}->{got_ret}"
    )]
    InternalDynFnDowncastError {
        expected_arg: TypeInfo,
        expected_ret: TypeInfo,
        got_arg: TypeInfo,
        got_ret: TypeInfo,
    },
}
