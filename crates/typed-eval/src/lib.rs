#![cfg_attr(feature = "nightly", feature(min_specialization))]

// helps derived macro refer to this crate by name, from within the same crate
extern crate self as typed_eval;

mod compiler;
mod dyn_fn;
mod error;
mod errors;
mod eval_type;
mod expr;
mod parser;
mod span;
mod type_info;

pub use compiler::*;
pub use dyn_fn::*;
pub use error::*;
pub use errors::*;
pub use eval_type::*;
pub use expr::*;
pub use parser::*;
pub use span::*;
pub use type_info::*;
pub use typed_eval_macro::{EvalType, eval_type_methods};

#[cfg(feature = "nightly")]
#[rustversion::not(nightly)]
compile_error!("The `nightly` feature requires a nightly compiler");
