use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Error, FnArg, GenericArgument, ImplItem, ItemImpl, PatType, PathArguments,
    Receiver, Result, ReturnType, Type, TypePath, TypeReference,
};

pub fn eval_type_methods_impl(impl_block: ItemImpl) -> Result<TokenStream> {
    let self_ty = &impl_block.self_ty;

    let mut method_regs = Vec::new();

    for item in impl_block.items.iter() {
        if let ImplItem::Fn(f) = item {
            let mut inputs = f.sig.inputs.iter();

            if !matches!(
                inputs.next(),
                Some(FnArg::Receiver(Receiver {
                    reference: Some(_),
                    ..
                }))
            ) {
                return Err(Error::new_spanned(
                    &f.sig,
                    "expected first argument to be &self",
                ));
            }

            let arg_types = inputs
                .map(|arg| {
                    if let FnArg::Typed(PatType { ty, .. }) = arg {
                        Ok(remove_ref(ty))
                    } else {
                        Err(Error::new_spanned(arg, "expected typed argument"))
                    }
                })
                .collect::<Result<Vec<_>>>()?;

            let fn_name = &f.sig.ident;
            let fn_name_str = fn_name.to_string();
            let arg_count = arg_types.len();
            let register_fn =
                format_ident!("register_method_call_{}", arg_count);
            let output_type = match &f.sig.output {
                ReturnType::Default => syn::parse_quote!(()),
                ReturnType::Type(_, ty) => remove_ref(ty).clone(),
            };

            let expanded = {
                quote! {
                    registry.#register_fn::<#(#arg_types,)* #output_type>(#fn_name_str, Self::#fn_name)?;
                }
            };

            method_regs.push(expanded);
        }
    }

    let expanded = quote! {
        #impl_block

        impl typed_eval::EvalTypeMethods for #self_ty {
            fn register_methods<Ctx: typed_eval::EvalType>(
                mut registry: typed_eval::RegistryAccess<Ctx, Self>,
            ) -> typed_eval::Result<()> {
                #(#method_regs)*
                Ok(())
            }
        }
    };

    Ok(expanded.into())
}

fn remove_ref(ty: &Type) -> Type {
    match ty {
        // &T --> T
        Type::Reference(TypeReference { elem, .. }) => elem.as_ref().clone(),

        // &Cow<str> --> String
        Type::Path(type_path) if is_cow(type_path) => {
            let inner_ty = cow_type(type_path);
            let owned_ty: Type =
                syn::parse_quote!(<#inner_ty as ToOwned>::Owned);
            owned_ty
        }

        _ => ty.clone(),
    }
}

fn is_cow(type_path: &syn::TypePath) -> bool {
    type_path.qself.is_none()
        && type_path
            .path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "Cow")
}

// extract SomeType from Cow<'_, SomeType>
fn cow_type(type_path: &TypePath) -> &Type {
    assert!(is_cow(type_path));

    let last_seg = type_path.path.segments.last().unwrap();
    let PathArguments::AngleBracketed(args) = &last_seg.arguments else {
        panic!("Expected AngleBracketed arguments")
    };
    for arg in &args.args {
        // skip lifetimes, return first type argument
        if let GenericArgument::Type(ty) = arg {
            return ty;
        }
    }

    panic!("Expected to have at least one Type generic argument")
}
