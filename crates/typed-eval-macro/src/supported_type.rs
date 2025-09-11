use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, DeriveInput, Fields};

pub fn supported_type_derive_impl(ast: &DeriveInput) -> TokenStream {
    let struct_name = &ast.ident;

    let Data::Struct(DataStruct {
        fields: Fields::Named(fields),
        ..
    }) = &ast.data
    else {
        panic!(
            "#[derive(SupportedType) only supports structs with named fields"
        )
    };

    let register_fields = fields.named.iter().map(|field| {
        let field_name = field
            .ident
            .as_ref()
            .expect("it is a struct with named fields");
        let field_name_str = field_name.to_string();
        let field_type = &field.ty;

        quote! {
            registry.register_field_access::<#field_type>(
                #field_name_str,
                |obj| obj.#field_name.to_ref_type(),
            )?;
        }
    });

    TokenStream::from(quote! {
        impl typed_eval::SupportedType for #struct_name {
            type RefType<'a> = &'a Self;

            fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
                self
            }

            fn register<Ctx: typed_eval::SupportedType>(
                mut registry: typed_eval::RegistryAccess<Ctx, Self>,
            ) -> Result<(), String> {
                #(
                    #register_fields
                )*

                Ok(())
            }
        }
    })
}
