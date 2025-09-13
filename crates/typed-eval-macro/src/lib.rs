#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]

mod supported_type;
mod supported_type_methods;

use proc_macro::TokenStream;
use supported_type::supported_type_derive_impl;
use supported_type_methods::supported_type_methods_impl;
use syn::{DeriveInput, ItemImpl, parse_macro_input};

#[proc_macro_derive(SupportedType, attributes(typed_eval))]
pub fn supported_type_derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    supported_type_derive_impl(&ast)
}

#[proc_macro_attribute]
pub fn supported_type_methods(
    _attr: TokenStream,
    item: TokenStream,
) -> TokenStream {
    let impl_block = parse_macro_input!(item as ItemImpl);

    match supported_type_methods_impl(impl_block) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}
