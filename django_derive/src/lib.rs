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
    Traversed,
    Ignored,
    Sort(Option<syn::LitStr>),
    ForeignKey(syn::LitStr),
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
            },
            "exclude" => {
                Ok(DjangoItem::Ignored)
            },
            "traverse" => {
                Ok(DjangoItem::Traversed)
            },
            "op" => {
                // op(in = MyInOperatorClass)
                let content;
                let _: syn::token::Paren = syn::parenthesized!(content in input);
                let punc = syn::punctuated::Punctuated::<DjangoOperator, syn::Token![,]>::parse_terminated(&content)?;
                Ok(DjangoItem::Operators(punc.into_iter().collect()))
            }
            "sort" => {
                if input.lookahead1().peek(syn::token::Paren) { 
                    let content;
                    let _: syn::token::Paren = syn::parenthesized!(content in input);
                    let item = content.parse()?;
                    Ok(DjangoItem::Sort(Some(item)))
                } else {
                    Ok(DjangoItem::Sort(None))
                }
            },
            "foreign_key" => {
                let _: syn::Token![=] = input.parse()?;
                let key = input.parse()?;
                Ok(DjangoItem::ForeignKey(key))
            },
            _ => Err(syn::Error::new_spanned(
                attr,
                "unsupported django attribute",
            )),
        }
    }
}

#[derive(Debug)]
enum DjangoFiltering {
    Included {
        default_operator: (Option<syn::Ident>, Option<syn::Path>),
        operators: BTreeMap<String, Option<syn::Path>>,
    },
    Traversed, 
    Excluded,
}

#[derive(Debug)]
enum DjangoCell {
    Scalar,
    ForeignRow(syn::LitStr),
    Excluded
}

#[derive(Debug)]
struct DjangoMeta {
    name: Option<syn::LitStr>,
    filtering: DjangoFiltering,
    sort: Option<Option<syn::LitStr>>,
    cell: DjangoCell,
}

impl syn::parse::Parse for DjangoMeta {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut field_name = None;
        let mut operators = BTreeMap::new();
        let mut defop = (None, None);
        let mut excluded = false;
        let mut traversed = false;
        let punc =
            syn::punctuated::Punctuated::<DjangoItem, syn::Token![,]>::parse_terminated(input)?;
        let mut sort = None;
        let mut foreign_key = None;
        
        for item in punc {
            match item {
                DjangoItem::Rename(new_name) => {
                    field_name = Some(new_name);
                },
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
                },
                DjangoItem::DefaultOperator2(fun) => {
                    defop = (None, Some(fun));
                },
                DjangoItem::DefaultOperator1(op) => {
                    defop = (Some(op), None);
                },
                DjangoItem::Ignored => {
                    excluded = true;
                }
                DjangoItem::Traversed => {
                    traversed = true;
                },
                DjangoItem::Sort(key) => {
                    sort = Some(key)
                },
                DjangoItem::ForeignKey(key) => {
                    foreign_key = Some(key)
                }
            }
        }
        let filtering = if excluded {
            DjangoFiltering::Excluded
        } else if traversed {
            DjangoFiltering::Traversed
        } else {
            DjangoFiltering::Included {
                default_operator: defop,
                operators
            }
        };
        let cell = if excluded {
            DjangoCell::Excluded
        } else if let Some(key) = foreign_key {
            DjangoCell::ForeignRow(key)
        } else {
            DjangoCell::Scalar
        };

        Ok( Self { filtering, sort, cell, name: field_name } )
    }
}

#[proc_macro_derive(Queryable, attributes(django))]
pub fn queryable(input: TokenStream) -> TokenStream {
    let syn::DeriveInput {
        ident,
        data,
        generics,
        ..
    } = syn::parse_macro_input!(input);
    
    let mut body = pm2::TokenStream::new();
    let mut structs = pm2::TokenStream::new();

    let builtin_operators = BTreeMap::<_, syn::Path>::from([
        ("eq", syn::parse_quote! {::django_query::operators::Eq}),
        ("in", syn::parse_quote! {::django_query::operators::In}),
        ("lt", syn::parse_quote! {::django_query::operators::Less}),
        ("lte", syn::parse_quote! {::django_query::operators::LessEq}),
        ("gt", syn::parse_quote! {::django_query::operators::Greater}),
        ("gte", syn::parse_quote! {::django_query::operators::GreaterEq}),
        ("contains", syn::parse_quote! {::django_query::operators::Contains}),
        ("icontains", syn::parse_quote! {::django_query::operators::IContains}),
        (
            "startswith",
            syn::parse_quote! {::django_query::operators::StartsWith},
        ),
        ("endswith", syn::parse_quote! {::django_query::operators::EndsWith}),
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
                        let parsed = attr.parse_args::<DjangoMeta>().expect("failed to parse django attribute");
                        match parsed.filtering {
                            DjangoFiltering::Included { default_operator, operators: djoperators } => {
                                for (key, value) in djoperators {
                                    if let Some(value) = value {
                                        operators.insert(key, value);
                                    } else {
                                        operators.insert(
                                            key.clone(),
                                            builtin_operators.get(key.as_str()).unwrap().clone(),
                                        );
                                    }
                                }
                                match default_operator {
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
                            },
                            DjangoFiltering::Traversed => {
                                traversed = true;
                            },
                            DjangoFiltering::Excluded => {
                                excluded = true;
                            },
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
            panic!("Queryable can only be derived for structs with named fields.");
        }
    } else {
        panic!("Queryable can only be derived for structs with named fields.");
    }

    let res: TokenStream = quote::quote! {
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
    }
    .into();

    res
}

#[proc_macro_derive(Sortable, attributes(django))]
pub fn sortable(input: TokenStream) -> TokenStream {
    let syn::DeriveInput {
        ident,
        data,
        generics,
        ..
    } = syn::parse_macro_input!(input);

    let mut body = pm2::TokenStream::new();
    let mut structs = pm2::TokenStream::new();
    
    let wc = generics.where_clause.as_ref();

    if let syn::Data::Struct(s) = data {
        if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = s.fields {
            for field in named.iter() {
                let fieldid = field.ident.as_ref().unwrap();
                let mut fieldname = syn::LitStr::new(&fieldid.to_string(), fieldid.span());
                let mut sort = None;
                
                for attr in field.attrs.iter() {
                    if attr.path.is_ident("django") {
                        let parsed = attr.parse_args::<DjangoMeta>().expect("failed to parse django attribute");
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
                        impl #generics ::django_query::ordering::Field<#ident #generics> for #structname #wc {
                            type Value = #fieldtype;
                            fn value<'a>(&self, data: &'a #ident #generics) -> &'a Self::Value {
                                &data.#fieldid
                            }
                        }
                    });
                    if let Some(key) = sort {
                        body.extend(quote::quote! {
                            visitor.visit_key_sort(#fieldname, &#structname, #key);
                        });
                    } else {
                        body.extend(quote::quote! {
                            visitor.visit_sort(#fieldname, &#structname);
                        });
                    }
                }
            }
        } else {
            panic!("Sortable can only be derived for structs with named fields.");
        }
    } else {
        panic!("Sortable can only be derived for structs with named fields.");
    }

    let res: TokenStream = quote::quote! {
        const _: () = {
            #structs
            #[automatically_derived]
            impl #generics ::django_query::Sortable for #ident #generics #wc {
                fn accept_visitor<V: ::django_query::ordering::SortVisitor<Self>>(visitor: &mut V)
                where
                    Self: Sized
                {
                    #body
                }
            }
        };
    }
    .into();

    res
}

#[proc_macro_derive(IntoRow, attributes(django))]
pub fn into_row(input: TokenStream) -> TokenStream {
    let syn::DeriveInput {
        ident,
        data,
        generics,
        ..
    } = syn::parse_macro_input!(input);

    let mut cells = pm2::TokenStream::new();
    let mut cols = pm2::TokenStream::new();
    
    let wc = generics.where_clause.as_ref();

    if let syn::Data::Struct(s) = data {
        if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = s.fields {
            for field in named.iter() {
                let fieldid = field.ident.as_ref().unwrap();
                let mut fieldname = syn::LitStr::new(&fieldid.to_string(), fieldid.span());
                let fieldtype = &field.ty;

                let mut excluded = false;
                let mut key = None;
                
                for attr in field.attrs.iter() {
                    if attr.path.is_ident("django") {
                        let parsed = attr.parse_args::<DjangoMeta>().expect("failed to parse django attribute");
                        let cell = parsed.cell;
                        match cell {
                            DjangoCell::Excluded => { excluded = true; },
                            DjangoCell::ForeignRow(fkey) => {
                                key = Some(fkey)
                            },
                            DjangoCell::Scalar => {
                            }
                        }
                        if let Some(name) = parsed.name {
                            fieldname = name;
                        }
                    }
                }
                if !excluded {
                    if let Some(key) = key {
                        cells.extend(quote::quote! {
                            visitor.visit_value(#fieldname, <#fieldtype as ::django_query::row::AsForeignKey>::as_foreign_key(&self.#fieldid, #key));
                        });
                    } else {
                        cells.extend(quote::quote! {
                            visitor.visit_value(#fieldname, <#fieldtype as ::django_query::row::IntoCellValue>::to_cell_value(&self.#fieldid));
                        });
                    }
                    cols.extend(quote::quote! {
                        visitor.visit_column(#fieldname);
                    });
                }
            }
        } else {
            panic!("IntoRow can only be derived for structs with named fields.");
        }
    } else {
        panic!("IntoRow can only be derived for structs with named fields.");
    }

    
    let res: TokenStream = quote::quote! {
        const _: () = {
            #[automatically_derived]
            impl #generics ::django_query::IntoRow for #ident #generics #wc {
                fn accept_cell_visitor<V: ::django_query::row::CellVisitor>(&self, visitor: &mut V)
                {
                    #cells
                }
                fn accept_column_visitor<V: ::django_query::row::ColumnVisitor>(visitor: &mut V)
                {
                    #cols
                }
            }
        };
    }
    .into();

    res
}

