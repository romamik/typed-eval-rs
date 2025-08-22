use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, Fields};

#[proc_macro_derive(SupportedType)]
pub fn supported_type_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    supported_type_derive_impl(&ast)
}

fn supported_type_derive_impl(ast: &syn::DeriveInput) -> TokenStream {
    let struct_name = &ast.ident;

    let Data::Struct(DataStruct {
        fields: Fields::Named(fields),
        ..
    }) = &ast.data
    else {
        panic!("#[derive(SupportedType) only supports structs with named fields")
    };

    let register_fields = fields.named.iter().map(|field| {
        let field_name = field.ident.as_ref();
        quote! { compiler.register_field(stringify!(#field_name), |obj: &Self| &obj.#field_name); }
    });

    TokenStream::from(quote! {
        impl typed_eval::SupportedType for #struct_name {
            fn register<Arg: typed_eval::SupportedType>(compiler: &mut typed_eval::CompilerInner<Arg>) {
                #(
                    #register_fields
                )*
            }

            fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> typed_eval::DynBoxedFn {
                typed_eval::DynBoxedFn::make_ret_ref(getter)
            }
        }
    })
}
