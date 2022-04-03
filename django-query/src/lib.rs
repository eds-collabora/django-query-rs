//! # django-query
//!
//! This crate is a toolkit for assembling a mock instance of a
//! Django-style API, although some of the parts may be useful beyond
//! just mocking. The main tools provided are:
//!
//! - The [Filterable] trait, and its derive macro, which allow you to
//!   use attribute markup to automatically parse Django-style filter
//!   URLs into filter objects.
//!
//! - The [Sortable] trait, and its derive macro, which allow you to
//!   use attribute markup to automatically parse Django-style
//!   ordering URLs into sorter objects.
//!
//! - The [IntoRow] trait, and its derive macro, which allow you to
//!   create Django-style JSON responses for your type; note that this
//!   isn't just standard serialization because complex objects are
//!   replaced by one of their fields, which functions as a foreign
//!   key.
//!
//! - The [mock::Endpoint] type, which implements [wiremock::Respond],
//!   and can provide a mock endpoint for a collection of objects
//!   whose type implements the preceding three traits.
//!
//! Example:
//! ```rust
//! use django_query::{IntoRow, Filterable, Sortable, mock::Endpoint};
//! use std::sync::Arc;
//! use wiremock::{Mock, MockServer, matchers, http::Url};
//!
//! #[derive(IntoRow, Filterable, Sortable)]
//! struct Foo {
//!     #[django(sort, op(in, icontains, iexact))]
//!     name: String,
//!     #[django(sort, op(lt, gt))]
//!     value: i32
//! }
//!
//! #[derive(IntoRow, Filterable, Sortable)]
//! struct Bar {
//!     #[django(op(icontains, startswith))]
//!     names: Vec<String>,
//!     #[django(traverse, foreign_key="name")]
//!     foo: Arc<Foo>
//! }
//!
//! tokio_test::block_on( async {
//!    let server = MockServer::start().await;
//!    let bars = Arc::new(vec![
//!        Bar {
//!          names: vec! [
//!            "apple".to_string(),
//!            "banana".to_string()
//!          ],
//!          foo: Arc::new(Foo {
//!            name: "foo1".to_string(),
//!            value: 5,
//!          }),
//!        },
//!        Bar {
//!          names: vec! [
//!            "carrot".to_string(),
//!          ],
//!          foo: Arc::new(Foo {
//!            name: "foo2".to_string(),
//!            value: 4,
//!          }),
//!       },
//!    ]);
//!
//!    Mock::given(matchers::method("GET"))
//!         .respond_with(Endpoint::new(bars, Some(&server.uri())))
//!         .mount(&server)
//!         .await;
//!
//!    // Let's build up a Django-style query URL to test; first the
//!    // mock server's base URI
//!    let mut url = Url::parse(&server.uri())
//!        .expect("failed to parse MockServer URL");
//!    // Now request all only Bars that have a name in names that contains
//!    // "PL" (case insensitive)
//!    url.query_pairs_mut().append_pair("names__icontains", "PL");
//!    // Request only Bars whose foo has a value greater than 4
//!    url.query_pairs_mut().append_pair("foo__value__gt", "4");
//!    let body: serde_json::Value = reqwest::get(url)
//!        .await
//!        .expect("error getting response")
//!        .json()
//!        .await
//!        .expect("error parsing response");
//!
//!    assert_eq!(body, serde_json::json!{
//!      {
//!        "count": 1,
//!        "next": null,
//!        "prev": null,
//!        "results": [
//!          { "names": ["apple", "banana"], "foo": "foo1" },
//!        ]
//!      }
//!    });
//! });
//! ```
pub mod filtering;
pub mod mock;
pub mod operators;
pub mod ordering;
pub mod row;

pub use crate::filtering::{Filterable, OperatorSet};
pub use crate::operators::Scalar;
pub use crate::ordering::{OrderingSet, Sortable};
pub use crate::row::{IntoCellValue, IntoRow, StringCellValue};
pub use django_derive::{Filterable, IntoRow, Sortable};
