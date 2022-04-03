use proc_macro::{self, TokenStream};

use proc_macro2 as pm2;

mod attributes;
mod filtering;
mod ordering;
mod row;

/// Derive the `Filterable` trait, creating suitable `FilterClass`
/// types.
///
/// This is only implemented for structs with named fields. All fields
/// will be exposed, with a default operator of `exact` unless annotated
/// to indicate otherwise. The annotations use the `django` attribute,
/// which has the following options:
///
/// - `#[django(rename="new_name")]` Expose the annotated member for
///   filtering as `new_name instead of using its name in the source
///   code.
///
/// - `#[django(default_op=iexact)]` Set the default operator, which
///   is applied when the field is referred to directly to be `iexact`,
///   where `iexact` can be replaced with any of the built-in operators
///   included in this crate.
///
/// - `#[django(default_fun=my_crate::MyOperatorClass)]` Set the
///   default operator to be the custom type
///   `my_crate::MyOperatorClass`, which must implement
///   `OperatorClass`.]
///
/// - `#[django(exclude)]` Do not expose this field, it cannot be used
///   in filtering.
///
/// - `#[django(traverse)]` This type of this field is itself `Filterable`
///   and nested filters onto its members are permitted via the double
///   underscore syntax that Django uses.
///
/// - `#[django(op(in, icontains))]` In addition to the default operator,
///   this field can also be filtered on using `in` and `icontains`, using
///   double underscores to separate the operator from the field name.
///
/// - `#[django(op(foo=my_crate::MyOperatorClass))]` This field has a
///   custom filter operator `foo` which can be appended to its name
///   with double underscores, and which when used, creates a filter
///   using `my_crate::MyOperatorClass`, which must itself be an
///   instance of `OperatorClass`.
#[proc_macro_derive(Filterable, attributes(django))]
pub fn filterable(input: TokenStream) -> TokenStream {
    let derive: syn::DeriveInput = syn::parse_macro_input!(input);

    let res: pm2::TokenStream = filtering::derive_filterable(derive);

    res.into()
}

/// Derive the `Sortable` trait, creating suitable `SorterClass` types.
///
/// This is only implemented for structs with named fields. No fields
/// will be available to use as sort orders, unless there are
/// annotations to indicate otherwise. The annotations use the
/// `django` attribute, which has the following significant options
/// here:
///
/// - `#[django(rename="new_name")]` Expose the annotated member for
///   sorting as `new_name instead of using its name in the source
///   code.
///
/// - `#[django(sort)]` The field, which must be [Ord], will be
///   exposed as a sort order for the enclosing type. The ordering
///   is taken directly from [Ord].
///
/// - `#[django(sort="name","age")]` The field has a type which is
///   itself `Sortable`. Expose this field as defining a sort order of
///   the same name, and When sorting by this field make the order
///   defined by the field's own member `name`, and then by its own
///   member `age` as a secondary sort.
#[proc_macro_derive(Sortable, attributes(django))]
pub fn sortable(input: TokenStream) -> TokenStream {
    let derive: syn::DeriveInput = syn::parse_macro_input!(input);

    let res: pm2::TokenStream = ordering::derive_sortable(derive);

    res.into()
}

/// Derive the `IntoRow` trait, determining the display of nested
/// objects.
///
/// This is only implemented for structs with named fields. All fields
/// will be included in the output by default, which means they must
/// have types which implement `IntoCellValue`. The annotations for
/// this derive macro use the `django` attribute, which has the
/// following significant options here:
///
/// - `#[django(rename="new_name")]` Expose the annotated member in
///    the output `new_name instead of using its name in the source code.
///
/// - `#[django(exclude)]` Do not include the annotated member in any
///   output.
///
/// - `#[django(foreign_key="field_name")]` The field has a type which
///   is itself `IntoRow`. Rather than requiring the field's type to
///   implement `IntoCellValue`, instead take the value of the field
///   from the cell called `field_name` in the field's own type's
///   output row.
#[proc_macro_derive(IntoRow, attributes(django))]
pub fn into_row(input: TokenStream) -> TokenStream {
    let derive: syn::DeriveInput = syn::parse_macro_input!(input);

    let res: pm2::TokenStream = row::derive_into_row(derive);

    res.into()
}
