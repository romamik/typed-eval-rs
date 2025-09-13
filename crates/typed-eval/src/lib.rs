#![cfg_attr(feature = "nightly", feature(min_specialization))]

// helps derived macro refer to this crate by name, from within the same crate
extern crate self as typed_eval;

mod compiler;
mod compiler_registry;
mod dyn_fn;
mod eval_type;
mod expr;
mod expr_parser;

pub use compiler::*;
pub use compiler_registry::*;
pub use dyn_fn::*;
pub use eval_type::*;
pub use expr::*;
pub use expr_parser::*;

pub use typed_eval_macro::{EvalType, eval_type_methods};

#[cfg(feature = "nightly")]
#[rustversion::not(nightly)]
compile_error!("The `nightly` feature requires a nightly compiler");
