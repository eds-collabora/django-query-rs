#![cfg_attr(docsrs, feature(doc_cfg))]

//! # django-query
//!
//! This crate is a toolkit for assembling a mock instance of a
//! Django-style API, although some of the parts may be useful beyond
//! just mocking. The tools provided depend on the available features.
//!
//! There are the following features:
//! - `"filter"` - Filtering values of a type using django filters.
//! - `"sort"` - Sort values of a type using django orderings.
//! - `"row"` - Convert a type into a row in tabular JSON output.
//! - `"wiremock"` - Wiremock endpoints to expose data; implies `"filter"`, `"row"` and `"sort"`.
#![cfg_attr(
    feature = "persian-rug",
    doc = r##"
 - `"persian-rug"` - Support for types built with the [`persian-rug`](::persian_rug) crate.
"##
)]
#![cfg_attr(
    not(feature = "persian-rug"),
    doc = r##"
 - `"persian-rug"` - Support for types built with the `persian-rug` crate.
"##
)]
#![cfg_attr(
    feature = "clone-replace",
    doc = r##"
 - `"clone-replace"` - Use a [`CloneReplace`](clone_replace::CloneReplace) from the [`clone-replace`](::clone_replace) crate to provide data to mocks.
"##
)]
#![cfg_attr(
    not(feature = "clone-replace"),
    doc = r##"
 - `"clone-replace"` - Use a `CloneReplace` from the `clone-replace` crate to provide data to mocks.
"##
)]
//!
#![cfg_attr(
    feature = "filter",
    doc = r##"
## Filtering

The [`Filterable`](filtering::Filterable) trait, and its derive macro,
which allow you to use attribute markup to automatically parse
Django-style filter URLs into filter objects, when the `"filter"`
feature is enabled.

Example:
```rust
use django_query::filtering::{Filterable, OperatorSet};

#[derive(Filterable)]
struct Foo {
    #[django(op(lt, gt))]
    a: i32
}

let os = OperatorSet::<Foo>::new();
let filter = os.create_filter_from_query("a__lt=4").unwrap();
assert!(filter.filter_one(&Foo { a: 3}));
```

"##
)]
#![cfg_attr(
    feature = "sort",
    doc = r##"
## Sorting

The [`Sortable`](sorting::Sortable) trait, and its derive macro, which
allow you to use attribute markup to automatically parse Django-style
ordering URLs into sorter objects, when the `"sort"` feature is
enabled.

Example:
```rust
use core::cmp::Ordering;
use django_query::sorting::{OrderingSet, Sortable};

#[derive(Sortable)]
struct Foo {
    #[django(sort)]
    a: i32
}

let os = OrderingSet::<Foo>::new();
let sort = os.create_sort("-a").unwrap();
assert_eq!(sort.compare(&Foo { a: 3}, &Foo {a: 4}), Ordering::Greater);
```

"##
)]
#![cfg_attr(
    feature = "row",
    doc = r##"

## Tabular Output

The [`IntoRow`](row::IntoRow) trait, and its derive macro, which allow
you to create Django-style JSON responses for your type; note that
this isn't just standard serialization because complex objects are
replaced by one of their fields, which functions as a foreign
key. This is available when the `"row"` feature is enabled.

Example:
```rust
use django_query::row::{IntoRow, Serializer};
use serde_json::json;

#[derive(IntoRow)]
struct Foo {
    a: Vec<i32>
}

let ser = Foo::get_serializer();
let f = Foo { a: vec![2, 3]};
assert_eq!(ser.to_json(&f), json! {
  { "a": [2, 3] }
});
```

"##
)]
#![cfg_attr(
    feature = "wiremock",
    doc = r##"

## Mock Endpoints

The [`Endpoint`](mock::Endpoint) type, which implements
[`wiremock::Respond`], and can provide a mock endpoint for a
collection of objects whose type implements the preceding three
traits. This is available when the `"wiremock"` feature is enabled.

Example:
```rust
use django_query::{filtering::Filterable, mock::Endpoint, row::IntoRow, sorting::Sortable};
use wiremock::{Mock, MockServer, matchers, http::Url};

#[derive(Clone, IntoRow, Filterable, Sortable)]
struct Foo {
    name: String,
}

# tokio_test::block_on( async {
let server = MockServer::start().await;
let foos = vec![
  Foo { name: "foo1".to_string() },
  Foo { name: "foo2".to_string() }
];

Mock::given(matchers::method("GET"))
     .respond_with(Endpoint::new(foos, Some(&server.uri())))
     .mount(&server)
     .await;

let url = Url::parse(&server.uri()).expect("failed to parse MockServer URL");

let body: serde_json::Value = reqwest::get(url)
    .await
    .expect("error getting response")
    .json()
    .await
    .expect("error parsing response");

assert_eq!(body, serde_json::json!{
  {
    "count": 2,
    "next": null,
    "previous": null,
    "results": [
      { "name": "foo1" },
      { "name": "foo2" },
    ]
  }
});

# });
```
"##
)]
#![cfg_attr(
    any(
        feature = "row",
        feature = "filter",
        feature = "sort",
        feature = "wiremock"
    ),
    doc = r##"

## Context support

There is support throughout this crate for types that require some
context value in order to perform processing with them. Each module
has an additional entry point with context support:
"##
)]
#![cfg_attr(
    feature = "filter",
    doc = r##"
- [`FilterableWithContext`](filtering::FilterableWithContext) in [`filtering`].
"##
)]
#![cfg_attr(
    feature = "sort",
    doc = r##"
- [`SortableWithContext`](sorting::SortableWithContext) in [`sorting`].
"##
)]
#![cfg_attr(
    feature = "row",
    doc = r##"
- [`IntoRowWithContext`](row::IntoRowWithContext) in [`row`].
"##
)]
#![cfg_attr(
    feature = "wiremock",
    doc = r##"
- [`EndpointWithContext`](mock::EndpointWithContext`) in [`mock`].
"##
)]
#![cfg_attr(
    any(
        feature = "row",
        feature = "filter",
        feature = "sort",
        feature = "wiremock"
    ),
    doc = r##"

The context support makes no more than basic assumptions about what the
context value is, or how it behaves. 
"##
)]
#![cfg_attr(
    any(feature = "row", feature = "filter", feature = "sort"),
    doc = r##"
However, corresponding
derive macros for the traits are not provided, for the same reason.
"##
)]
#![cfg_attr(
    all(
        feature = "persian-rug",
        any(feature = "row", feature = "filter", feature = "sort")
    ),
    doc = r##"

The `"persian-rug"` feature provides derive macros for using a [`persian_rug::Context`][::persian_rug::Context]:
for types that require it:"##
)]
#![cfg_attr(
    all(feature = "persian-rug", feature = "filter"),
    doc = r##"
- [`FilterableWithPersianRug`](filtering::FilterableWithPersianRug),
"##
)]
#![cfg_attr(
    all(feature = "persian-rug", feature = "sort"),
    doc = r##"
- [`SortableWithPersianRug`](sorting::SortableWithPersianRug)
"##
)]
#![cfg_attr(
    all(feature = "persian-rug", feature = "sort"),
    doc = r##"
- [`IntoRowWithPersianRug`](row::IntoRowWithPersianRug).
"##
)]
#![cfg_attr(
    all(
        feature = "persian-rug",
        any(feature = "row", feature = "filter", feature = "sort")
    ),
    doc = r##"

Note that there is no corresponding `PersianRug` trait, only a derive
macro which produces an implementation for the generic (`WithContext`)
trait.
"##
)]
#![cfg_attr(
    all(feature = "clone-replace", feature = "wiremock"),
    doc = r##"

## Mutable collection support

Ideally, the data served by the mock endpoints using this crate should
be mutable, so that testing can examine how code reacts as the data
store evolves. This crate uses a [`RowSource`](mock::RowSource) trait to
represent something that:
- Can be locked or queried to obtain a fixed view of the data.
- For which references to that retrieved fixed view are iterable.

This probably cannot be fully abstracted in stable Rust as it stands
at the moment of writing, because of the lack of generic associated
types.

The standard [`RowSource`](mock::RowSource) implementations are for
[`Arc<Vec<T>>`](::std::sync::Arc) and [`Vec<T>`], which clone
themselves on each request (the latter being provided because it is
convenient for small tests, even though it is expensive). Neither
permits mutability.

However, if the `"clone-replace"` feature is enabled, a
[`CloneReplace`](::clone_replace::CloneReplace) from the
[`clone-replace`](::clone_replace) crate can be used as a
[`RowSource`](mock::RowSource).  Note that in addition to the
implementation for
[`CloneReplace<Vec<T>>`](::clone_replace::CloneReplace), there is also
a
[`CloneReplaceFieldSource`](mock::clone_replace::CloneReplaceFieldSource)
which allows you to extract a [`Vec`] field from a structure which is
entirely contained within a
[`CloneReplace`](::clone_replace::CloneReplace).

"##
)]
#![cfg_attr(
    all(
        feature = "clone-replace",
        feature = "wiremock",
        feature = "persian-rug"
    ),
    doc = r##"

If the `"persian-rug"` feature is enabled, then in combination with
`"clone-replace"` it allows you to use individual tables from a
[`persian_rug::Context`](::persian_rug::Context) wrapped in a
[`CloneReplace`](::clone_replace::CloneReplace) as data sources via
[`CloneReplacePersianRugTableSource`](mock::clone_replace::persian_rug::CloneReplacePersianRugTableSource).

"##
)]

#[cfg(feature = "filter")]
#[cfg_attr(docsrs, doc(cfg(feature = "filter")))]
pub mod filtering;

#[cfg(feature = "wiremock")]
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub mod mock;

#[cfg(feature = "sort")]
#[cfg_attr(docsrs, doc(cfg(feature = "sort")))]
pub mod sorting;

#[cfg(feature = "row")]
#[cfg_attr(docsrs, doc(cfg(feature = "row")))]
pub mod row;

#[cfg(feature = "persian-rug")]
#[doc(hidden)]
pub mod persian_rug;
