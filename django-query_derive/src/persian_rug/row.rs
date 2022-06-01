use proc_macro2 as pm2;

use super::helpers::get_persian_rug_constraints;
use crate::attributes::{DjangoCell, DjangoMeta};

pub fn derive_into_row_with_persian_rug(input: syn::DeriveInput) -> pm2::TokenStream {
    let syn::DeriveInput {
        attrs,
        ident: ty_ident,
        data,
        generics,
        ..
    } = input;

    let mut cells = pm2::TokenStream::new();
    let mut cols = pm2::TokenStream::new();

    let full_generics = generics.clone();
    let (_, ty_generics, wc) = generics.split_for_impl();

    let (context, used_types) = match get_persian_rug_constraints(&attrs) {
        Ok(v) => v,
        Err(e) => {
            return e.to_compile_error();
        }
    };

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
                        let parsed = match attr.parse_args::<DjangoMeta>() {
                            Ok(parsed) => parsed,
                            Err(e) => {
                                return syn::Error::into_compile_error(e);
                            }
                        };
                        let cell = parsed.cell;
                        match cell {
                            DjangoCell::Excluded => {
                                excluded = true;
                            }
                            DjangoCell::ForeignRow(fkey) => key = Some(fkey),
                            DjangoCell::Scalar => {}
                        }
                        if let Some(name) = parsed.name {
                            fieldname = name;
                        }
                    }
                }
                if !excluded {
                    if let Some(key) = key {
                        cells.extend(quote::quote! {
                            visitor.visit_value(
                                #fieldname,
                                <<#fieldtype as ::django_query::row::AsForeignKeyWithContext<'_, 'r, A>>::CellReducer as ::django_query::row::CellReducer<'_, 'r, #fieldtype>>::reduce_to_cell(&<#fieldtype as ::django_query::row::AsForeignKeyWithContext<'_, 'r, A>>::get_cell_reducer(self.access.clone(), #key), &value.#fieldid)
                            );
                        });
                    } else {
                        cells.extend(quote::quote! {
                            visitor.visit_value(#fieldname, <#fieldtype as ::django_query::row::IntoCellValue>::to_cell_value(&value.#fieldid));
                        });
                    }
                    cols.extend(quote::quote! {
                        visitor.visit_column(#fieldname);
                    });
                }
            }
        } else {
            return syn::Error::new(
                ty_ident.span(),
                "IntoRowWithPersianRug can only be derived for structs with named fields.",
            )
            .to_compile_error();
        }
    } else {
        return syn::Error::new(
            ty_ident.span(),
            "IntoRowWithPersianRug can only be derived for structs with named fields.",
        )
        .to_compile_error();
    }

    let generics_with_lifespan = if full_generics.params.is_empty() {
        syn::parse_quote! { <'r, A> }
    } else {
        let mut g = full_generics;
        g.params
            .push(syn::GenericParam::Lifetime(syn::LifetimeDef::new(
                syn::Lifetime::new("'r", pm2::Span::call_site()),
            )));
        g.params
            .push(syn::GenericParam::Type(syn::parse_quote! { A }));
        g
    };

    let fk_generics = {
        let mut g = generics_with_lifespan.clone();
        g.params
            .push(syn::GenericParam::Lifetime(syn::LifetimeDef::new(
                syn::Lifetime::new("'a", pm2::Span::call_site()),
            )));
        g
    };

    let mut constraints = pm2::TokenStream::new();
    constraints.extend(quote::quote! {
        context = #context,
    });
    let used_types: syn::punctuated::Punctuated<syn::Type, syn::Token![,]> =
        used_types.into_iter().collect();
    constraints.extend(quote::quote! {
        access(#used_types)
    });

    let mut wc = if let Some(wc) = wc {
        wc.clone()
    } else {
        syn::WhereClause {
            where_token: Default::default(),
            predicates: syn::punctuated::Punctuated::new(),
        }
    };

    wc.predicates.push(syn::parse_quote! {
        A: ::persian_rug::Accessor<Context = #context> + 'r
    });

    // requires:
    // + Add A to generics
    // + Add 'r to generics
    // + Define constraints variable for pr constraints
    // + Extend wc
    // + definition of cells
    let res = quote::quote! {
        const _: () = {
            #[automatically_derived]
            #[persian_rug::constraints(#constraints)]
            impl #generics_with_lifespan ::django_query::row::IntoRowWithContext<'r, A> for #ty_ident #ty_generics #wc
            {
                type Serializer = Serializer<A>;
                fn get_serializer(access: A) -> Self::Serializer {
                     Serializer { access }
                }
            }

            #[automatically_derived]
            #[persian_rug::constraints(#constraints)]
            impl #fk_generics ::django_query::row::AsForeignKeyWithContext<'a, 'r, A> for #ty_ident #ty_generics #wc
            {
                type CellReducer = ::django_query::row::SerializerCellReducer<'a, Serializer<A>>;
                fn get_cell_reducer(access: A, key: &'a str) -> Self::CellReducer {
                    ::django_query::row::SerializerCellReducer::new(Serializer { access }, key)
                }
            }

            pub struct Serializer<A: ::persian_rug::Accessor> {
                access: A
            }

            #[automatically_derived]
            #[persian_rug::constraints(#constraints)]
            impl #generics_with_lifespan ::django_query::row::Serializer<'r, #ty_ident #ty_generics> for Serializer<A> #wc
            {
                fn accept_cell_visitor<V: ::django_query::row::CellVisitor>(&self, value: &#ty_ident #ty_generics, visitor: &mut V)
                {
                    #cells
                }
                fn accept_column_visitor<V: ::django_query::row::ColumnVisitor>(&self, visitor: &mut V)
                {
                    #cols
                }
            }
        };
    };

    res
}
