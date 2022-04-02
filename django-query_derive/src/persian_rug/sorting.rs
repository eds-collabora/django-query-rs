use proc_macro2 as pm2;

use super::helpers::get_persian_rug_constraints;
use crate::attributes::DjangoMeta;

pub fn derive_sortable_with_persian_rug(input: syn::DeriveInput) -> pm2::TokenStream {
    let syn::DeriveInput {
        attrs,
        ident,
        data,
        generics,
        ..
    } = input;

    let mut body = pm2::TokenStream::new();
    let mut structs = pm2::TokenStream::new();

    let full_generics = generics.clone();
    let (_, ty_generics, wc) = generics.split_for_impl();

    let (context, used_types) = match get_persian_rug_constraints(&attrs) {
        Ok(v) => v,
        Err(e) => {
            return e.to_compile_error();
        }
    };

    let generics_with_lifespan = if full_generics.params.is_empty() {
        syn::parse_quote! { <'s> }
    } else {
        let mut g = full_generics;
        g.params
            .push(syn::GenericParam::Lifetime(syn::LifetimeDef::new(
                syn::Lifetime::new("'s", pm2::Span::call_site()),
            )));
        g.params
            .push(syn::GenericParam::Type(syn::parse_quote! { A }));
        g
    };

    let mut wc = if let Some(wc) = wc {
        wc.clone()
    } else {
        syn::WhereClause {
            where_token: Default::default(),
            predicates: syn::punctuated::Punctuated::new(),
        }
    };

    wc.predicates.push(syn::parse_quote! {
        A: ::persian_rug::Accessor<Context = #context> + 's
    });

    let mut constraints = pm2::TokenStream::new();
    constraints.extend(quote::quote! {
        context = #context,
    });
    let used_types: syn::punctuated::Punctuated<syn::Type, syn::Token![,]> =
        used_types.into_iter().collect();
    constraints.extend(quote::quote! {
        access(#used_types)
    });

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
                        struct #structname<A> {
                            access: A
                        }

                        #[automatically_derived]
                        #[persian_rug::constraints(#constraints)]
                        impl #generics_with_lifespan ::django_query::sorting::Field<#ident #ty_generics> for #structname<A> #wc {
                            type Value = #fieldtype;
                            fn apply_sorter<V: ::django_query::sorting::Sorter<Self::Value>>(&self, op: &V, a: &#ident #ty_generics, b: &#ident #ty_generics) -> core::cmp::Ordering {
                                op.compare(&a.#fieldid, &b.#fieldid)
                            }
                        }
                    });
                    if let Some(key) = sort {
                        body.extend(quote::quote! {
                            visitor.visit_key_sort(
                                #fieldname,
                                &#structname {
                                    access: self.access.clone()
                                },
                                #key,
                                <#fieldtype as ::django_query::sorting::SortableWithContext<'s, A>>::get_meta(self.access.clone())
                            );
                        });
                    } else {
                        body.extend(quote::quote! {
                            visitor.visit_sort(
                                #fieldname,
                                &#structname {
                                    access: self.access.clone()
                                },
                                &::django_query::sorting::CompareClass
                            );
                        });
                    }
                }
            }
        } else {
            return syn::Error::new(
                ident.span(),
                "SortableWithPersianRug can only be derived for structs with named fields.",
            )
            .to_compile_error();
        }
    } else {
        return syn::Error::new(
            ident.span(),
            "SortableWithPersianRug can only be derived for structs with named fields.",
        )
        .to_compile_error();
    }

    let res = quote::quote! {
        const _: () = {
            #structs
            #[automatically_derived]
            #[persian_rug::constraints(#constraints)]
            impl #generics_with_lifespan ::django_query::sorting::SortableWithContext<'s, A> for #ident #ty_generics #wc {
                type Meta = Meta<A>;

                fn get_meta(access: A) -> Self::Meta {
                    Meta { access }
                }
            }

            pub struct Meta<A: ::persian_rug::Accessor> {
                access: A
            }

            #[persian_rug::constraints(#constraints)]
            impl #generics_with_lifespan ::django_query::sorting::Meta<'s, #ident #ty_generics> for Meta<A> #wc {
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
