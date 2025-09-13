#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]

mod eval_type;
mod eval_type_methods;

use eval_type::eval_type_derive_impl;
use eval_type_methods::eval_type_methods_impl;
use proc_macro::TokenStream;
use syn::{DeriveInput, ItemImpl, parse_macro_input};

#[proc_macro_derive(EvalType, attributes(typed_eval))]
pub fn eval_type_derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eval_type_derive_impl(&ast)
}

#[proc_macro_attribute]
pub fn eval_type_methods(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let impl_block = parse_macro_input!(item as ItemImpl);

    match eval_type_methods_impl(impl_block) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}
