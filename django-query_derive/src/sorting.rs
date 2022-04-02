use proc_macro2 as pm2;

use crate::attributes::DjangoMeta;

pub fn derive_sortable(input: syn::DeriveInput) -> pm2::TokenStream {
    let syn::DeriveInput {
        ident,
        data,
        generics,
        ..
    } = input;

    let mut body = pm2::TokenStream::new();
    let mut structs = pm2::TokenStream::new();

    let full_generics = generics.clone();
    let (generics, ty_generics, wc) = generics.split_for_impl();

    if let syn::Data::Struct(s) = data {
        if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = s.fields {
            for field in named.iter() {
                let fieldid = field.ident.as_ref().unwrap();
                let mut fieldname = syn::LitStr::new(&fieldid.to_string(), fieldid.span());
                let mut sort = None;

                for attr in field.attrs.iter() {
                    if attr.path.is_ident("django") {
                        let parsed = match attr.parse_args::<DjangoMeta>() {
                            Ok(parsed) => parsed,
                            Err(e) => {
                                return syn::Error::into_compile_error(e);
                            }
                        };
                        if let Some(name) = parsed.name {
                            fieldname = name;
                        }
                        if let Some(s) = parsed.sort {
                            sort = Some(s);
                        }
                    }
                }

                if let Some(sort) = sort {
                    let structname =
                        syn::Ident::new(&format!("{}Field", fieldid), pm2::Span::call_site());
                    let fieldtype = &field.ty;

                    structs.extend(quote::quote! {
                        #[derive(Clone)]
                        struct #structname;
                        #[automatically_derived]
                        impl #generics ::django_query::sorting::Accessor<#ident #ty_generics> for #structname #wc {
                            type Value = #fieldtype;
                            fn value<'a>(&self, data: &'a #ident #ty_generics) -> &'a Self::Value {
                                &data.#fieldid
                            }
                        }
                        impl ::django_query::sorting::ReferenceField for #structname #wc {}
                    });
                    if let Some(key) = sort {
                        body.extend(quote::quote! {
                            visitor.visit_key_sort(#fieldname, &#structname, #key, <#fieldtype as ::django_query::sorting::Sortable>::get_meta());
                        });
                    } else {
                        body.extend(quote::quote! {
                            visitor.visit_sort(#fieldname, &#structname, &::django_query::sorting::CompareClass);
                        });
                    }
                }
            }
        } else {
            return syn::Error::new(
                ident.span(),
                "Sortable can only be derived for structs with named fields.",
            )
            .to_compile_error();
        }
    } else {
        return syn::Error::new(
            ident.span(),
            "Sortable can only be derived for structs with named fields.",
        )
        .to_compile_error();
    }

    let generics_with_lifespan = if full_generics.params.is_empty() {
        syn::parse_quote! { <'s> }
    } else {
        let mut g = full_generics;
        g.params
            .push(syn::GenericParam::Lifetime(syn::LifetimeDef::new(
                syn::Lifetime::new("'s", pm2::Span::call_site()),
            )));
        g
    };

    let res = quote::quote! {
        const _: () = {
            #structs
            #[automatically_derived]
            impl #generics_with_lifespan ::django_query::sorting::Sortable<'s> for #ident #ty_generics #wc {
                type Meta = Meta;

                fn get_meta() -> Self::Meta {
                    Meta
                }
            }

            pub struct Meta;

            impl #generics_with_lifespan ::django_query::sorting::Meta<'s, #ident #ty_generics> for Meta #wc {
                fn accept_visitor<V: ::django_query::sorting::SortVisitor<'s, Target=#ident #ty_generics>>(&self, visitor: &mut V)
                where
                    Self: Sized
                {
                    #body
                }
            }
        };
    };

    res
}
