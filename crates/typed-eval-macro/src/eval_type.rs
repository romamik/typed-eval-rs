use darling::{FromDeriveInput, FromField};
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, DeriveInput, Fields};

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(typed_eval))]
struct EvalTypeOpts {
    #[darling(default)]
    no_methods: bool,
}

#[derive(Debug, FromField)]
#[darling(attributes(typed_eval))]
struct EvalTypeField {
    #[darling(default)]
    ignore: bool,

    #[darling(default)]
    rename: Option<String>,
}

pub fn eval_type_derive_impl(ast: &DeriveInput) -> TokenStream {
    let struct_opts = EvalTypeOpts::from_derive_input(ast)
        .expect("Failed to parse struct attributes");

    let struct_name = &ast.ident;
    let struct_name_str = struct_name.to_string();

    // no_methods - generate an empty EvalTypeMethods impl for type
    // not needed on nightly
    let methods_impl = struct_opts.no_methods.then(|| {
        #[cfg(feature = "nightly")]
        {
            use syn::spanned::Spanned;

            let attrs_span = ast
                .attrs
                .iter()
                .fold(struct_name.span(), |a, b| a.join(b.span()).unwrap());
            proc_macro::Diagnostic::spanned(
                attrs_span.unwrap(),
                proc_macro::Level::Note,
                "#[typed_eval(no_methods)] is not needed with feature=`nightly`"
            )
            .emit();
            quote! {}
        }

        #[cfg(not(feature = "nightly"))]
        quote! {
            impl typed_eval::EvalTypeMethods for #struct_name {}
        }
    });

    let Data::Struct(DataStruct {
        fields: Fields::Named(fields),
        ..
    }) = &ast.data
    else {
        panic!("#[derive(EvalType) only supports structs with named fields")
    };

    let register_fields = fields.named.iter().map(|field| {
        let field_opts = EvalTypeField::from_field(field)
            .expect("failed to parse field attributes");

        if field_opts.ignore {
            return quote! {};
        }

        let field_name = field
            .ident
            .as_ref()
            .expect("it is a struct with named fields");
        let field_name_str =
            field_opts.rename.unwrap_or_else(|| field_name.to_string());
        let field_type = &field.ty;

        quote! {
            registry.register_field_access::<#field_type>(
                #field_name_str,
                |obj| obj.#field_name.to_ref_type(),
            )?;
        }
    });

    TokenStream::from(quote! {
        #methods_impl

        impl typed_eval::EvalType for #struct_name {
            type RefType<'a> = &'a Self;

            fn type_info() -> typed_eval::TypeInfo {
                typed_eval::TypeInfo::new::<Self>(|| #struct_name_str.into())
            }

            fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
                self
            }

            fn register<Ctx: typed_eval::EvalType>(
                mut registry: typed_eval::RegistryAccess<Ctx, Self>,
            ) -> typed_eval::Result<()> {
                #(
                    #register_fields
                )*

                Ok(())
            }
        }
    })
}
