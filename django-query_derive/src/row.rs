use proc_macro2 as pm2;

use crate::attributes::{DjangoCell, DjangoMeta};

pub fn derive_into_row(input: syn::DeriveInput) -> pm2::TokenStream {
    let syn::DeriveInput {
        ident,
        data,
        generics,
        ..
    } = input;

    let mut cells = pm2::TokenStream::new();
    let mut cols = pm2::TokenStream::new();

    let full_generics = generics.clone();
    let (_, ty_generics, wc) = generics.split_for_impl();

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
                            visitor.visit_value(#fieldname, ::django_query::row::CellReducer::reduce_to_cell(&<#fieldtype as ::django_query::row::AsForeignKey>::get_cell_reducer(#key), &self.#fieldid));
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
            return syn::Error::new(
                ident.span(),
                "IntoRow can only be derived for structs with named fields.",
            )
            .to_compile_error();
        }
    } else {
        return syn::Error::new(
            ident.span(),
            "IntoRow can only be derived for structs with named fields.",
        )
        .to_compile_error();
    }

    let generics_with_lifespan = if full_generics.params.is_empty() {
        syn::parse_quote! { <'r> }
    } else {
        let mut g = full_generics;
        g.params
            .push(syn::GenericParam::Lifetime(syn::LifetimeDef::new(
                syn::Lifetime::new("'r", pm2::Span::call_site()),
            )));
        g
    };

    let res = quote::quote! {
        const _: () = {
            #[automatically_derived]
            impl #generics_with_lifespan ::django_query::row::SelfSerializer<'r> for #ident #ty_generics #wc {
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
    };

    res
}
