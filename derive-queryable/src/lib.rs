use std::collections::BTreeMap;
use syn::ext::IdentExt;

use proc_macro::{self, TokenStream};

use proc_macro2 as pm2;

#[derive(Debug)]
enum DjangoOperator {
    Standard(syn::Ident),
    Custom(syn::Ident, syn::Path),
}

impl syn::parse::Parse for DjangoOperator {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let op = input.call(syn::Ident::parse_any)?;
        if input.lookahead1().peek(syn::Token![=]) {
            let _: syn::Token![=] = input.parse()?;
            let fun = input.call(syn::Path::parse_mod_style)?;
            Ok(DjangoOperator::Custom(op, fun))
        } else {
            Ok(DjangoOperator::Standard(op))
        }
    }
}

#[derive(Debug)]
enum DjangoItem {
    Rename(syn::LitStr),
    Operators(Vec<DjangoOperator>),
    DefaultOperator1(syn::Ident),
    DefaultOperator2(syn::Path),
}

impl syn::parse::Parse for DjangoItem {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let attr: syn::Ident = input.parse()?;
        match attr.to_string().as_str() {
            "rename" => {
                // rename = "MyString"
                let _: syn::Token![=] = input.parse()?;
                let new_name: syn::LitStr = input.parse()?;
                Ok(DjangoItem::Rename(new_name))
            }
            "default_op" => {
                let _: syn::Token![=] = input.parse()?;
                let op = input.call(syn::Ident::parse_any)?;
                Ok(DjangoItem::DefaultOperator1(op))
            }
            "default_fun" => {
                let _: syn::Token![=] = input.parse()?;
                let fun = input.call(syn::Path::parse_mod_style)?;
                Ok(DjangoItem::DefaultOperator2(fun))
            }
            "op" => {
                // op(in = MyInOperatorClass)
                let content;
                let _: syn::token::Paren = syn::parenthesized!(content in input);
                let punc = syn::punctuated::Punctuated::<DjangoOperator, syn::Token![,]>::parse_terminated(&content)?;
                Ok(DjangoItem::Operators(punc.into_iter().collect()))
            }
            _ => Err(syn::Error::new_spanned(
                attr,
                "unsupported django attribute",
            )),
        }
    }
}

#[derive(Debug)]
struct DjangoMeta {
    pub name: Option<syn::LitStr>,
    pub default_operator: (Option<syn::Ident>, Option<syn::Path>),
    pub operators: BTreeMap<String, Option<syn::Path>>,
}

impl syn::parse::Parse for DjangoMeta {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut field_name = None;
        let mut operators = BTreeMap::new();
        let mut defop = (None, None);
        let punc =
            syn::punctuated::Punctuated::<DjangoItem, syn::Token![,]>::parse_terminated(input)?;

        for item in punc {
            match item {
                DjangoItem::Rename(new_name) => {
                    field_name = Some(new_name);
                }
                DjangoItem::Operators(ops) => {
                    for op in ops {
                        match op {
                            DjangoOperator::Standard(name) => {
                                operators.insert(name.to_string(), None);
                            }
                            DjangoOperator::Custom(name, fun) => {
                                operators.insert(name.to_string(), Some(fun));
                            }
                        }
                    }
                }
                DjangoItem::DefaultOperator2(fun) => {
                    defop = (None, Some(fun));
                }
                DjangoItem::DefaultOperator1(op) => {
                    defop = (Some(op), None);
                }
            }
        }
        Ok(Self {
            name: field_name,
            default_operator: defop,
            operators,
        })
    }
}

#[proc_macro_derive(Queryable, attributes(django))]
pub fn go(input: TokenStream) -> TokenStream {
    let syn::DeriveInput {
        ident,
        data,
        generics,
        ..
    } = syn::parse_macro_input!(input);

    let mut body = pm2::TokenStream::new();
    let mut structs = pm2::TokenStream::new();

    let builtin_operators = BTreeMap::<_, syn::Path>::from([
        ("eq", syn::parse_quote! {crate::operators::Eq}),
        ("in", syn::parse_quote! {crate::operators::In}),
        ("lt", syn::parse_quote! {crate::operators::Less}),
        ("lte", syn::parse_quote! {crate::operators::LessEq}),
        ("gt", syn::parse_quote! {crate::operators::Greater}),
        ("gte", syn::parse_quote! {crate::operators::GreaterEq}),
        ("contains", syn::parse_quote! {crate::operators::Contains}),
        ("icontains", syn::parse_quote! {crate::operators::IContains}),
        (
            "startswith",
            syn::parse_quote! {crate::operators::StartsWith},
        ),
        ("endswith", syn::parse_quote! {crate::operators::EndsWith}),
    ]);

    let wc = generics.where_clause.as_ref();

    if let syn::Data::Struct(s) = data {
        if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = s.fields {
            for field in named.iter() {
                let fieldid = field.ident.as_ref().unwrap();
                let mut fieldname = syn::LitStr::new(&fieldid.to_string(), fieldid.span());
                let mut operators = BTreeMap::new();
                let mut defop = None;

                for attr in field.attrs.iter() {
                    if attr.path.is_ident("django") {
                        let dj: DjangoMeta =
                            attr.parse_args().expect("failed to parse django attribute");
                        if let Some(name) = dj.name {
                            fieldname = name;
                        }
                        for (key, value) in dj.operators {
                            if let Some(value) = value {
                                operators.insert(key, value);
                            } else {
                                operators.insert(
                                    key.clone(),
                                    builtin_operators.get(key.as_str()).unwrap().clone(),
                                );
                            }
                        }
                        match dj.default_operator {
                            (Some(op), None) => {
                                defop = Some(
                                    builtin_operators
                                        .get(op.to_string().as_str())
                                        .unwrap()
                                        .clone(),
                                )
                            }
                            (None, Some(fun)) => defop = Some(fun),
                            _ => {}
                        }
                    }
                }
                let fieldtype = &field.ty;
                let structname =
                    syn::Ident::new(&format!("{}Field", fieldid), pm2::Span::call_site());

                structs.extend(quote::quote! {
                    #[derive(Clone)]
                    struct #structname;
                    #[automatically_derived]
                    impl #generics Field<#ident #generics> for #structname #wc {
                        type Value = #fieldtype;
                        fn value<'a>(&self, data: &'a #ident #generics) -> &'a #fieldtype {
                            &data.#fieldid
                        }
                    }
                });

                let defop = if let Some(op) = defop {
                    op
                } else {
                    syn::parse_quote! {crate::operators::Eq}
                };

                body.extend(quote::quote! {
                    let mut qf = QueryableField::new(FilterClassImpl::new(#structname, #defop));
                });
                for (key, value) in operators {
                    body.extend(quote::quote! {
                        qf.add_operator(#key, FilterClassImpl::new(#structname, #value));
                    });
                }
                body.extend(quote::quote! {
                    qr.add_field(#fieldname, qf);
                })
            }
        } else {
            panic!("Queryable can only be derived for structs with named fields.");
        }
    } else {
        panic!("Queryable can only be derived for structs with named fields.");
    }

    let res: TokenStream = quote::quote! {
        const _: () = {
            #structs
            #[automatically_derived]
            impl #generics Queryable for #ident #generics #wc {
                fn create_metadata() -> QueryableRecord<Self> {
                    let mut qr = QueryableRecord::new();
                    #body
                    qr
                }
            }
        };
    }
    .into();

    res
}
