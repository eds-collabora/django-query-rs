use std::collections::BTreeMap;

use proc_macro2 as pm2;

use crate::attributes::{DjangoFiltering, DjangoMeta};

pub fn derive_queryable(input: syn::DeriveInput) -> pm2::TokenStream {
    let syn::DeriveInput {
        ident,
        data,
        generics,
        ..
    } = input;

    let mut body = pm2::TokenStream::new();
    let mut structs = pm2::TokenStream::new();

    let builtin_operators = BTreeMap::<_, syn::Path>::from([
        ("eq", syn::parse_quote! {::django_query::operators::Eq}),
        ("in", syn::parse_quote! {::django_query::operators::In}),
        ("lt", syn::parse_quote! {::django_query::operators::Less}),
        ("lte", syn::parse_quote! {::django_query::operators::LessEq}),
        ("gt", syn::parse_quote! {::django_query::operators::Greater}),
        (
            "gte",
            syn::parse_quote! {::django_query::operators::GreaterEq},
        ),
        (
            "contains",
            syn::parse_quote! {::django_query::operators::Contains},
        ),
        (
            "icontains",
            syn::parse_quote! {::django_query::operators::IContains},
        ),
        (
            "startswith",
            syn::parse_quote! {::django_query::operators::StartsWith},
        ),
        (
            "endswith",
            syn::parse_quote! {::django_query::operators::EndsWith},
        ),
        (
            "isnull",
            syn::parse_quote! {::django_query::operators::IsNull},
        ),
    ]);

    let wc = generics.where_clause.as_ref();

    if let syn::Data::Struct(s) = data {
        if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = s.fields {
            for field in named.iter() {
                let fieldid = field.ident.as_ref().unwrap();
                let mut fieldname = syn::LitStr::new(&fieldid.to_string(), fieldid.span());
                let mut operators = BTreeMap::new();
                let mut defop = None;
                let mut excluded = false;
                let mut traversed = false;

                for attr in field.attrs.iter() {
                    if attr.path.is_ident("django") {
                        let parsed = match attr.parse_args::<DjangoMeta>() {
                            Ok(parsed) => parsed,
                            Err(e) => return syn::Error::into_compile_error(e),
                        };
                        match parsed.filtering {
                            DjangoFiltering::Included {
                                default_operator,
                                operators: djoperators,
                            } => {
                                for (key, value) in djoperators {
                                    if let Some(value) = value {
                                        operators.insert(key.to_string(), value);
                                    } else if let Some(op) =
                                        builtin_operators.get(key.to_string().as_str())
                                    {
                                        operators.insert(key.to_string(), op.clone());
                                    } else {
                                        return syn::Error::new(
                                            key.span(),
                                            format!("unknown operator {}", key),
                                        )
                                        .to_compile_error();
                                    }
                                }
                                match default_operator {
                                    (Some(op), None) => {
                                        if let Some(op) =
                                            builtin_operators.get(op.to_string().as_str())
                                        {
                                            defop = Some(op.clone());
                                        } else {
                                            return syn::Error::new(
                                                op.span(),
                                                format!("unknown operator {}", op),
                                            )
                                            .to_compile_error();
                                        }
                                    }
                                    (None, Some(fun)) => defop = Some(fun),
                                    _ => {}
                                }
                            }
                            DjangoFiltering::Traversed => {
                                traversed = true;
                            }
                            DjangoFiltering::Excluded => {
                                excluded = true;
                            }
                        };
                        if let Some(name) = parsed.name {
                            fieldname = name;
                        }
                    }
                }
                if excluded {
                    continue;
                }

                let fieldtype = &field.ty;
                let structname =
                    syn::Ident::new(&format!("{}Field", fieldid), pm2::Span::call_site());

                let mut fieldbody = pm2::TokenStream::new();

                for (key, value) in operators {
                    fieldbody.extend(quote::quote! {
                        visitor.visit_operator(#key, self, #value);
                    })
                }

                structs.extend(quote::quote! {
                    #[derive(Clone)]
                    struct #structname;
                    #[automatically_derived]
                    impl #generics ::django_query::filtering::Field<#ident #generics> for #structname #wc {
                        type Value = #fieldtype;
                        fn apply<O: ::django_query::filtering::Operator<Self::Value>>(&self, op: &O, data: &#ident #generics) -> bool {
                            op.apply(&data.#fieldid)
                        }
                    }
                });

                if traversed {
                    body.extend(quote::quote! {
                        visitor.visit_record(#fieldname, &#structname, &<#fieldtype as ::django_query::filtering::Queryable>::get_meta());
                    });
                } else {
                    structs.extend(quote::quote! {
                        #[automatically_derived]
                        impl #generics ::django_query::filtering::Member<#ident #generics> for #structname #wc {
                            type Value = #fieldtype;
                            fn apply<O: ::django_query::filtering::Operator<<Self::Value as ::django_query::filtering::Operable>::Base>>(&self, op: &O, data: &#ident #generics) -> bool {
                                <Self::Value as ::django_query::filtering::Operable>::apply(&data.#fieldid, op)
                            }
                            fn accept_visitor<V: ::django_query::filtering::MemberVisitor<Self, #ident #generics, <Self as ::django_query::filtering::Field<#ident #generics>>::Value>>(&self, visitor: &mut V) {
                                #fieldbody
                            }
                        }
                    });

                    let defop = if let Some(op) = defop {
                        op
                    } else {
                        syn::parse_quote! {::django_query::operators::Eq}
                    };

                    body.extend(quote::quote! {
                        visitor.visit_member(#fieldname, &#structname, #defop);
                    });
                }
            }

            structs.extend(quote::quote! {
                pub struct MyRecord;
                impl #generics ::django_query::filtering::Record<#ident #generics> for MyRecord #wc {
                    fn accept_visitor<V: ::django_query::filtering::RecordVisitor<#ident #generics>>(&self, visitor: &mut V) where Self: Sized {
                        #body
                    }
                }
            });
        } else {
            return syn::Error::new(
                ident.span(),
                "Queryable can only be derived for structs with named fields.",
            )
            .to_compile_error();
        }
    } else {
        return syn::Error::new(
            ident.span(),
            "Queryable can only be derived for structs with named fields.",
        )
        .to_compile_error();
    }

    let res = quote::quote! {
        const _: () = {
            #structs
            #[automatically_derived]
            impl #generics ::django_query::Queryable for #ident #generics #wc {
                type Meta = MyRecord;
                fn get_meta() -> Self::Meta {
                    MyRecord
                }
            }
        };
    };

    res
}
