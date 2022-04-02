use proc_macro::{self, TokenStream};

use proc_macro2 as pm2;

mod attributes;
mod filtering;
mod ordering;
mod row;

#[proc_macro_derive(Filterable, attributes(django))]
pub fn queryable(input: TokenStream) -> TokenStream {
    let derive: syn::DeriveInput = syn::parse_macro_input!(input);

    let res: pm2::TokenStream = filtering::derive_filterable(derive);

    res.into()
}

#[proc_macro_derive(Sortable, attributes(django))]
pub fn sortable(input: TokenStream) -> TokenStream {
    let derive: syn::DeriveInput = syn::parse_macro_input!(input);

    let res: pm2::TokenStream = ordering::derive_sortable(derive);

    res.into()
}

#[proc_macro_derive(IntoRow, attributes(django))]
pub fn into_row(input: TokenStream) -> TokenStream {
    let derive: syn::DeriveInput = syn::parse_macro_input!(input);

    let res: pm2::TokenStream = row::derive_into_row(derive);

    res.into()
}
