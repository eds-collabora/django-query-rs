//! # Create Django-style endpoints using [`wiremock`].
//!
//! One possible use of this crate is to mock Django endpoints for
//! development or testing. This may be convenient if you are otherwise
//! developing in Rust, for example, and want to have CI run unit or
//! integration tests that involve calls to such a service.
//!
//! In order to be most useful as a mock, it needs to provide complete
//! integrated Django endpoints:
//! - a HTTP server listening for requests.
//! - a store of data to serve.
//! - request parsing to determine result sets.
//! - Django formatted, paginated output.
//!
//! The other modules in this crate provide most of this, when coupled
//! with [`wiremock`]. This module is mostly concerned with tying
//! everything together into a whole.
//!
//! # Overview
//!
//! The main type in this module is [`Endpoint`], which is an
//! implementor of [`wiremock::Respond`], and can be mounted directly
//! on a [`wiremock::MockServer`]. It takes a [`RowSource`] which is responsible
//! for providing a snapshot of data to serve for each request. Each [`Endpoint`]
//! serves values of one type, and that type must always implement:
//! - [`Sortable`] so that `"ordering"` requests can be processed.
//! - [`Filterable`] so that filtering request can be processed
//!   (`"__in"`, `"__lt"` and so forth).
//! - [`IntoRow`] so that rows of result data can be produced.
//!
//! All three traits must always be implemented, because of the way in
//! which cargo features interact - they are required to be stricly
//! additive and adding type bounds decreases the set of types are
//! permitted, and is thus subtractive.
//!
//! The main functionality this module handles itself is pagination, and
//! it provides a simple limit/offset model.
//!
//! Example
//! ```rust
//! use django_query::filtering::Filterable;
//! use django_query::mock::Endpoint;
//! use django_query::row::IntoRow;
//! use django_query::sorting::Sortable;
//! use std::sync::Arc;
//! use wiremock::{Mock, MockServer, matchers, http::Url};
//!
//! #[derive(IntoRow, Filterable, Sortable)]
//! struct Foo {
//!   #[django(sort, op(in, lt, gt))]
//!   a: i32
//! }
//!
//! let foos = (0..20i32).into_iter().map(|a| Foo { a }).collect::<Vec<_>>();
//!
//! # tokio_test::block_on( async {
//! let server = MockServer::start().await;
//!
//! Mock::given(matchers::method("GET"))
//!      .respond_with(Endpoint::new(Arc::new(foos), Some(&server.uri())))
//!      .mount(&server)
//!      .await;
//!
//! let u = format!("{}?limit=1&offset=5&a__lt=10&ordering=-a", server.uri());
//! let body: serde_json::Value = reqwest::get(&u)
//!     .await
//!     .expect("error getting response")
//!     .json()
//!     .await
//!     .expect("error parsing response");
//!
//! let prev = format!("{}/?limit=1&offset=4&a__lt=10&ordering=-a", server.uri());
//! let next = format!("{}/?limit=1&offset=6&a__lt=10&ordering=-a", server.uri());
//! assert_eq!(body, serde_json::json!{
//!   {
//!     "count": 10,
//!     "next": next,
//!     "previous": prev,
//!     "results": [
//!       { "a": 4 }
//!     ]
//!   }
//! });
//! # });
//! ```

use core::cmp::min;
use core::fmt::Debug;
use regex::{Captures, Regex};
use std::num::ParseIntError;
use std::str::FromStr;

use log::{debug, trace};
use thiserror::Error;
use wiremock::http::Url;
use wiremock::{Request, Respond, ResponseTemplate};

use crate::filtering::{
    Filter, Filterable, FilterableWithContext, OperatorSet, OperatorSetWithContext,
};
use crate::row::{IntoRow, IntoRowWithContext, Serializer};
use crate::sorting::{OrderingSet, OrderingSetWithContext, Sortable, SortableWithContext, Sorter};

#[derive(Debug, Error)]
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
/// An error produced by the mock endpoint.
pub enum MockError {
    /// A query parameter was provided that wasn't recognised.
    ///
    /// It was not a valid filtering operator, a pagination request,
    /// or an ordering expression.
    #[error("unknown query parameter `{0}`")]
    UnknownQueryParameter(String),
    /// Failed to parse an integer.
    ///
    /// This likely arises from limit/offset.
    #[error("expected integer value in query")]
    BadIntegerInQuery(#[from] ParseIntError),
    /// Failed to parse an enum value.
    #[error("bad value in query: {0}")]
    BadValue(#[from] strum::ParseError),
    /// Failed to parse a date.
    #[error("bad datetime value in query")]
    BadDateInQuery(#[from] chrono::ParseError),
    /// A bad sort expression was given.
    #[error("invalid sort: {0}")]
    BadSort(#[from] crate::sorting::SorterError),
}

struct ResponseSet<T> {
    contents: Vec<T>,
    total_matches: usize,
    next: Option<String>,
    prev: Option<String>,
}

impl<T> ResponseSet<T> {
    pub fn new(
        contents: Vec<T>,
        total_matches: usize,
        next: Option<String>,
        prev: Option<String>,
    ) -> Self {
        ResponseSet {
            contents,
            total_matches,
            next,
            prev,
        }
    }
}

impl<'q, T: IntoRow<'q>> ResponseSet<&T> {
    pub fn mock_json(&self) -> serde_json::Value {
        let mut map = serde_json::map::Map::new();
        map.insert(
            "count".to_string(),
            serde_json::Value::Number(serde_json::Number::from(self.total_matches)),
        );
        map.insert("results".to_string(), to_rows(&self.contents));
        map.insert(
            "next".to_string(),
            self.next
                .as_ref()
                .map(|x| serde_json::Value::String(x.clone()))
                .unwrap_or(serde_json::Value::Null),
        );
        map.insert(
            "previous".to_string(),
            self.prev
                .as_ref()
                .map(|x| serde_json::Value::String(x.clone()))
                .unwrap_or(serde_json::Value::Null),
        );

        serde_json::Value::Object(map)
    }
}

fn to_rows<'q, T: IntoRow<'q>>(data: &[&T]) -> serde_json::Value {
    let mut array = Vec::new();
    let ser = T::get_serializer();
    for item in data {
        array.push(ser.to_json(item));
    }
    serde_json::Value::Array(array)
}

struct Page {
    offset: usize,
    limit: usize,
}

struct PaginatedResponse<'a, T> {
    data: Vec<&'a T>,
    total: usize,
    next: Option<Page>,
    prev: Option<Page>,
}

struct ResponseSetBuilder<'a, T> {
    ordering: Vec<Box<dyn Sorter<T> + 'a>>,
    filtering: Vec<Box<dyn Filter<T> + 'a>>,
    limit: Option<usize>,
    offset: usize,
}

impl<'a, T> Default for ResponseSetBuilder<'a, T> {
    fn default() -> Self {
        Self::new(None)
    }
}

impl<'a, T> ResponseSetBuilder<'a, T> {
    pub fn new(default_limit: Option<usize>) -> Self {
        ResponseSetBuilder {
            ordering: Vec::new(),
            filtering: Vec::new(),
            limit: default_limit,
            offset: 0,
        }
    }

    pub fn order_by(&mut self, order: Box<dyn Sorter<T> + 'a>) -> &mut Self {
        self.ordering.push(order);
        self
    }

    pub fn filter_by(&mut self, filter: Box<dyn Filter<T> + 'a>) -> &mut Self {
        self.filtering.push(filter);
        self
    }

    pub fn limit(&mut self, limit: usize) -> &mut Self {
        self.limit = Some(limit);
        self
    }

    pub fn offset(&mut self, offset: usize) -> &mut Self {
        self.offset = offset;
        self
    }

    pub fn apply<'b, I: Iterator<Item = &'b T>>(&mut self, iter: I) -> PaginatedResponse<'b, T> {
        let mut v = Vec::new();
        for item in iter {
            v.push(item)
        }

        for f in &self.filtering {
            f.filter_ref_vec(&mut v);
        }

        for order in &self.ordering {
            order.sort_ref_vec(&mut v);
        }

        let limit = self.limit.unwrap_or(v.len());
        let total = v.len();

        let start = min(v.len(), self.offset);
        let prev = if start > 0 {
            Some(Page {
                offset: self.offset - min(self.offset, limit),
                limit,
            })
        } else {
            None
        };

        v.drain(..start);

        let end = min(v.len(), limit);
        let next = if end < v.len() {
            Some(Page {
                offset: self.offset + limit,
                limit,
            })
        } else {
            None
        };

        v.drain(end..);

        PaginatedResponse {
            data: v,
            total,
            next,
            prev,
        }
    }
}

fn make_page_url(url: &Url, offset: usize, limit: usize) -> Url {
    let mut new_url = url.clone();

    let mut new_pairs = new_url.query_pairs_mut();
    let old_pairs = url.query_pairs();

    let mut offset_seen = false;

    new_pairs.clear();
    for (key, value) in old_pairs {
        match key.as_ref() {
            "limit" => {
                new_pairs.append_pair("limit", &limit.to_string());
            }
            "offset" => {
                if offset > 0 {
                    new_pairs.append_pair("offset", &offset.to_string());
                }
                offset_seen = true;
            }
            _ => {
                new_pairs.append_pair(key.as_ref(), value.as_ref());
            }
        }
    }

    if !offset_seen && offset > 0 {
        new_pairs.append_pair("offset", &offset.to_string());
    }

    new_pairs.finish();
    drop(new_pairs);

    new_url
}

/// Provide a collection of objects on demand.
///
/// This trait generalises the idea of "something that can provide
/// data to query".  It's important to note that the data it contains
/// is owned, and is therefore guaranteed to survive for as long as
/// needed, in particular for the life of the query. For correctness,
/// it is expected that the returned data is a snapshot that will not
/// change.
///
/// The reason for this definition is to make it possible to return
/// views of data without copying, where that makes sense. For example
/// using `Arc<Vec<T>>` as a [`RowSource`] does not entail copying data.
/// It's possible to define mutable types that generate snapshots on
/// demand which also implement `RowSource`, if you need your test
/// data to evolve.
///
/// The main drawback of this definition is that supporting bare
/// containers becomes very expensive. [`Vec<T>`] is defined to be a
/// [`RowSource`], but the entire [`Vec`] must be copied for every call to
/// [`get`](RowSource::get).
///
#[cfg_attr(
    feature = "clone-replace",
    doc = r##"
The `"clone-replace"` feature enables a mutable [`RowSource`] using a
[`clone_replace::CloneReplace`](::clone_replace::CloneReplace). Your
test code is free to hold a reference to, and mutate, the
[`CloneReplace`](::clone_replace::CloneReplace) wrapped collection.
Each query will get an [`Arc<T>`](std::sync::Arc) of a snapshot of the
state of the collection at the start of the query. You can also
extract a single [`Vec<T>`] field from a struct and serve that using
[`CloneReplaceFieldSource`].
"##
)]
#[cfg_attr(
    all(feature = "clone-replace", feature = "persian-rug"),
    doc = r##"
When both `"clone-replace"` and `"persian-rug"` are enabled, you can
combine a [`CloneReplace`](::clone_replace::CloneReplace) and a
[`persian_rug::Context`] using a [`CloneReplacePersianRugTableSource`]
to create a [`RowSource`] for a single type inside the shared state, which
remains mutable.
"##
)]
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub trait RowSource
where
    Self::Rows: RowSet<Item = Self::Item>,
    for<'a> &'a <Self::Rows as RowSet>::Target: IntoIterator<Item = &'a Self::Item>,
{
    /// The type of the objects this provides.
    type Item;
    /// The type of the collection this provides.
    type Rows;
    // If this is unowned, then we can handle reference types
    // seamlessly and avoid copying entire vecs. But it _must_ be
    // owned for a wide variety of the more interesting cases
    // (e.g. MutexGuard, Arc) so we pay the price of copies in the
    // simple cases. I don't think it's possible to express the trait
    // bounds without GAT to handle both cases cleanly.  Actually,
    // it's worse than this: you can't implement Mutex at all because
    // MutexGuard is not fully owned: the lifetime parameter can't be
    // coped with because the HRTBs aren't powerful enough and GAT
    // isn't stable. So the best we can hope is probably fully owned
    // cases (which CloneReplace ought to provide).

    /// Return a new, owned collection of objects, which should now
    /// remain immutable.
    fn get(&self) -> Self::Rows;
}

/// An opaque snapshot produced by a [`RowSource`]
///
/// This trait represents an object that provides a fixed iterable
/// collection on demand. In order to express the idea that a
/// collection is iterable, and not consumed by being iterated over,
/// we must describe it as having [`IntoIterator`] implemented for
/// shared references. A [`RowSet`] in contrast is owned, so that the
/// snapshot can be repeatedly iterated over, without being consumed
/// or updated, as required.
pub trait RowSet {
    /// The iterable collection type this provides.
    type Target;
    /// The type of the items in the collection this provides.
    type Item;
    /// Produce a reference to an iterable collection.
    fn get(&self) -> &Self::Target;
}

impl<T: Clone> RowSource for Vec<T> {
    type Item = T;
    type Rows = Self;
    fn get(&self) -> Self::Rows {
        self.clone()
    }
}

impl<T> RowSet for Vec<T> {
    type Item = T;
    type Target = Self;
    fn get(&self) -> &Self::Target {
        self
    }
}

impl<T> RowSource for std::sync::Arc<Vec<T>> {
    type Item = T;
    type Rows = Self;
    fn get(&self) -> Self::Rows {
        self.clone()
    }
}

impl<T> RowSet for std::sync::Arc<Vec<T>> {
    type Item = T;
    type Target = Vec<T>;
    fn get(&self) -> &Self::Target {
        self
    }
}

#[cfg(all(feature = "clone-replace", feature = "persian-rug"))]
pub use self::clone_replace::persian_rug::CloneReplacePersianRugTableSource;
#[cfg(feature = "clone-replace")]
pub use self::clone_replace::CloneReplaceFieldSource;

#[doc(hidden)]
#[cfg(feature = "clone-replace")]
pub mod clone_replace {
    use super::{RowSet, RowSource};
    use ::clone_replace::CloneReplace;

    // Use a Vec wrapped in a CloneReplace as a RowSource.

    impl<T> RowSource for CloneReplace<Vec<T>> {
        type Item = T;
        type Rows = std::sync::Arc<Vec<T>>;

        fn get(&self) -> Self::Rows {
            self.access()
        }
    }

    /// Use a [`Vec`] valued field of a [`CloneReplace<T>`] as a [`RowSource`].
    pub struct CloneReplaceFieldSource<F, T> {
        data: CloneReplace<T>,
        getter: F,
    }

    impl<F, T, U> CloneReplaceFieldSource<F, T>
    where
        F: Fn(&T) -> &Vec<U> + Clone,
    {
        /// Create a new [`CloneReplaceFieldSource`].
        ///
        /// Here, `data` is the [`CloneReplace<T>`] that will provide
        /// the data, and `getter` is a lambda that can extract a
        /// [`Vec<U>`] from a `&T`.
        pub fn new(data: CloneReplace<T>, getter: F) -> Self {
            Self { data, getter }
        }
    }

    impl<F, T, U> RowSource for CloneReplaceFieldSource<F, T>
    where
        F: Fn(&T) -> &Vec<U> + Clone,
    {
        type Item = U;
        type Rows = ArcField<F, T>;
        fn get(&self) -> Self::Rows {
            ArcField {
                getter: self.getter.clone(),
                data: self.data.access(),
            }
        }
    }

    #[doc(hidden)]
    pub struct ArcField<F, T> {
        getter: F,
        data: std::sync::Arc<T>,
    }

    impl<F, T, U> RowSet for ArcField<F, T>
    where
        F: Fn(&T) -> &Vec<U>,
    {
        type Item = U;
        type Target = Vec<U>;
        fn get(&self) -> &Self::Target {
            (self.getter)(&self.data)
        }
    }

    #[cfg(feature = "persian-rug")]
    pub mod persian_rug {
        use super::{RowSet, RowSource};
        use clone_replace::CloneReplace;

        /// Use a table from a [`persian_rug::Context`] as a
        /// [`RowSource`]
        ///
        /// This [`RowSource`] requires an
        /// [`EndpointWithContext`](crate::mock::EndpointWithContext)
        /// to serve it. The [`RowSet`] conveniently doubles as the
        /// [`persian_rug::Accessor`] object in this implementation as
        /// required by
        /// [`EndpointWithContext`](crate::mock::EndpointWithContext).
        pub struct CloneReplacePersianRugTableSource<F, T> {
            data: CloneReplace<T>,
            getter: F,
        }

        impl<C, F, U> CloneReplacePersianRugTableSource<F, C>
        where
            F: Fn(&std::sync::Arc<C>) -> ::persian_rug::TableIterator<'_, U> + Clone,
            for<'a> &'a PersianRugTable<std::sync::Arc<C>, F>: IntoIterator<Item = &'a U>,
        {
            pub fn new(data: CloneReplace<C>, getter: F) -> Self {
                Self { data, getter }
            }
        }

        #[persian_rug::constraints(context = C, access(U))]
        impl<C, F, U> RowSource for CloneReplacePersianRugTableSource<F, C>
        where
            F: Fn(&std::sync::Arc<C>) -> ::persian_rug::TableIterator<'_, U> + Clone,
            for<'a> &'a PersianRugTable<std::sync::Arc<C>, F>: IntoIterator<Item = &'a U>,
        {
            type Item = U;
            type Rows = PersianRugTable<std::sync::Arc<C>, F>;
            fn get(&self) -> Self::Rows {
                PersianRugTable {
                    getter: self.getter.clone(),
                    access: self.data.access(),
                }
            }
        }

        #[doc(hidden)]
        #[derive(Clone)]
        pub struct PersianRugTable<A: Clone, F: Clone> {
            access: A,
            getter: F,
        }

        #[persian_rug::constraints(context = C, access(U))]
        impl<A, C, F, U> RowSet for PersianRugTable<A, F>
        where
            A: ::persian_rug::Accessor<Context = C>,
            F: Fn(&A) -> ::persian_rug::TableIterator<'_, U> + Clone,
        {
            type Item = U;
            type Target = Self;
            fn get(&self) -> &Self::Target {
                self
            }
        }

        impl<A, C, F> ::persian_rug::Accessor for PersianRugTable<A, F>
        where
            A: persian_rug::Accessor<Context = C>,
            C: persian_rug::Context,
            F: Clone,
        {
            type Context = C;

            fn get<T>(&self, what: &persian_rug::Proxy<T>) -> &T
            where
                Self::Context: persian_rug::Owner<T>,
                T: persian_rug::Contextual<Context = Self::Context>,
            {
                self.access.get(what)
            }

            fn get_iter<T>(&self) -> persian_rug::TableIterator<'_, T>
            where
                Self::Context: persian_rug::Owner<T>,
                T: persian_rug::Contextual<Context = Self::Context>,
            {
                self.access.get_iter()
            }

            fn get_proxy_iter<T>(&self) -> persian_rug::TableProxyIterator<'_, T>
            where
                Self::Context: persian_rug::Owner<T>,
                T: persian_rug::Contextual<Context = Self::Context>,
            {
                self.access.get_proxy_iter()
            }
        }

        #[persian_rug::constraints(context = C, access(U))]
        impl<'a, A, C, F, U> IntoIterator for &'a PersianRugTable<A, F>
        where
            A: persian_rug::Accessor<Context = C>,
            F: Fn(&'a A) -> persian_rug::TableIterator<'a, U> + Clone,
            U: 'a,
        {
            type Item = &'a U;
            type IntoIter = persian_rug::TableIterator<'a, U>;
            fn into_iter(self) -> Self::IntoIter {
                (self.getter)(&self.access)
            }
        }
    }
}

fn parse_query<'a, 'q, R, I>(
    input_url: &Url,
    output_url: &Url,
    iter: I,
    default_limit: Option<usize>,
) -> Result<ResponseSet<&'a R>, MockError>
where
    R: Filterable<'q>,
    // Note the 'q bound is a consequence of Sortable's implementation for StackedSorter in OrderingSet
    R: Sortable<'q> + 'q,
    R: IntoRow<'q>,
    I: Iterator<Item = &'a R>,
{
    let mut rb = ResponseSetBuilder::new(default_limit);
    let qr = OperatorSet::<R>::new();
    let sr = OrderingSet::<R>::new();
    let pairs = input_url.query_pairs();
    for (key, value) in pairs {
        match key.as_ref() {
            "ordering" => {
                rb.order_by(sr.create_sort(&*value)?);
            }
            "offset" => {
                let v = usize::from_str(value.as_ref())?;
                rb.offset(v);
            }
            "limit" => {
                let v = usize::from_str(value.as_ref())?;
                rb.limit(v);
            }
            _ => {
                if let Ok(filter) = qr.create_filter_from_query_pair(&key, &value) {
                    rb.filter_by(filter);
                    continue;
                }
                return Err(MockError::UnknownQueryParameter(String::from(key.as_ref())));
            }
        }
    }
    let response = rb.apply(iter);
    Ok(ResponseSet::new(
        response.data,
        response.total,
        response
            .next
            .map(|page| make_page_url(output_url, page.offset, page.limit).to_string()),
        response
            .prev
            .map(|page| make_page_url(output_url, page.offset, page.limit).to_string()),
    ))
}

/// A Django-style [`wiremock`] endpoint for a collection of objects.
///
/// This is the central type in this crate. [`Endpoint`] implements
/// [`wiremock::Respond`] and so can be mounted directly into a
/// [`wiremock::MockServer`]. It contains a [`RowSource`]
/// which it will query for a fresh data on each query.
///
/// The [`Endpoint`] will filter the returned data based on the query
/// parameters in the URL, by using the [`Filterable`] trait on the
/// objects. It will then sort the data using the [`Sortable`] trait
/// on the objects. It will paginate the results as required using
/// `"limit"` and `"offset"`. Finally, the returned data will be
/// converted using the [`IntoRow`] trait.
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub struct Endpoint<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
{
    row_source: T,
    base_uri: Option<Url>,
    default_limit: Option<usize>,
}

impl<'q, T> Endpoint<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
    <T as RowSource>::Item: Send + Sync + Filterable<'q> + Sortable<'q>,
{
    /// Create a new endpoint.
    ///
    /// `row_source` is where the data comes from. `base_uri`, if provided, should
    /// be the wiremock server URI.
    ///
    /// `base_uri` is only required because wiremock mangles the
    /// request URL such that it's not possible to inspect it to see
    /// how to give out other URLs on the same server; the port
    /// number, which is usually random when mocking, is lost. Since
    /// Django includes full absolute URLs for the next and preceding
    /// pages for a query, mimicking this is impossible without more
    /// information (i.e. including usable URLs for the next page
    /// within the query response is impossible).
    pub fn new(row_source: T, base_uri: Option<&str>) -> Self {
        Self {
            row_source,
            base_uri: base_uri.map(|x| Url::parse(x).unwrap()),
            default_limit: None,
        }
    }

    /// Set the default number of results returned
    ///
    /// This is the value used when no limit is specified in the
    /// query.  This value is configurable in Django; the default
    /// behaviour of this mock endpoint is to return everything, but
    /// that makes testing code that uses the default pagination more
    /// difficult.
    pub fn default_limit(&mut self, limit: usize) {
        self.default_limit = Some(limit);
    }
}

impl<'q, T> Respond for Endpoint<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
    // Note the 'q bound is a consequence of Sortable's implementation for StackedSorter in OrderingSet
    <T as RowSource>::Item: Send + Sync + Filterable<'q> + Sortable<'q> + IntoRow<'q> + 'q,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        // All of this is to work around the mess in wiremock that means
        // request.uri is inaccurate, and can't be used.
        let mut u = request.url.clone();
        if let Some(ref base) = self.base_uri {
            u.set_host(base.host_str()).unwrap();
            u.set_scheme(base.scheme()).unwrap();
            u.set_port(base.port()).unwrap();
        }
        let res = {
            let data = self.row_source.get();
            let res = {
                let rows = data.get().into_iter();
                let body = parse_query::<_, _>(&request.url, &u, rows, self.default_limit);
                match body {
                    Ok(rs) => {
                        let bb = rs.mock_json();
                        ResponseTemplate::new(200).set_body_json(bb)
                    }
                    Err(e) => {
                        debug!("Failed to respond to {}: {}", request.url, e);
                        ResponseTemplate::new(500).set_body_string(e.to_string())
                    }
                }
            };
            res
        };
        res
    }
}

/// Match a Django-style nested endpoint in wiremock
///
/// This avoids using regular expressions in the calling code. Here
/// - `root` is the base of the Django API, like `"/api/v0.2"`
/// - `parent` is the parent object, like `"bars"`
/// - `child` is the nested object, like `"foos"`
///
/// The above specification matches URL paths like
///
///    ``/api/v0.2/bars/1/foos``
///
/// See the description of [`NestedEndpointParams`] for more detail.
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub fn nested_endpoint_matches(root: &str, parent: &str, child: &str) -> impl wiremock::Match {
    wiremock::matchers::path_regex(format!(r"^{}/{}/[^/]+/{}/$", root, parent, child))
}

fn replace_into(target: &str, cap: &Captures<'_>) -> String {
    let mut res = String::new();
    cap.expand(target, &mut res);
    res
}

struct UrlTransform {
    regex: Regex,
    path: String,
    pairs: Vec<(String, String)>,
}

impl UrlTransform {
    pub fn new(pattern: &str, path: &str, pairs: Vec<(&str, &str)>) -> UrlTransform {
        Self {
            regex: Regex::new(pattern).unwrap(),
            path: path.to_string(),
            pairs: pairs
                .into_iter()
                .map(|(x, y)| (x.to_string(), y.to_string()))
                .collect(),
        }
    }

    pub fn transform(&self, url: &Url) -> Url {
        debug!("Matching {} against {:?}", url, self.regex);
        if let Some(captures) = self.regex.captures(url.path()) {
            let mut u = url.clone();
            u.set_path(&replace_into(&self.path, &captures));
            for (k, v) in self.pairs.iter() {
                u.query_pairs_mut()
                    .append_pair(&replace_into(k, &captures), &replace_into(v, &captures));
            }
            u
        } else {
            url.clone()
        }
    }
}

/// A Django nested route.
///
/// A nested route in Django is a way of specifying that a particular
/// filter field must be present when querying for a particular object
/// type, and that the filter type must be exact match. So for
/// example, if all objects of type `Foo` have a reference to a `Bar`,
/// and its too expensive or undesirable to query for `Foo`s without
/// specifying which `Bar` they come from, then you can express this
/// in Django as a nested route:
///
///   ``http://example.com/bars/1/foos``
///
/// where in order to ask any question about `Foo`s you must first
/// identify a particular associated `Bar`.
///
/// Since the django-query crate operates on query parameters, the
/// simplest way to handle this is to convert any relevant parts of
/// the path into query parameters as required, which is all this type
/// does. See its construction parameters [`NestedEndpointParams`] for
/// the details. This type is otherwise identical to [`Endpoint`].
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub struct NestedEndpoint<T> {
    transform: UrlTransform,
    row_source: T,
    base_uri: Option<Url>,
    default_limit: Option<usize>,
}

/// The construction parameters for [`NestedEndpoint`]
///
/// Example:
/// ```rust
/// # use django_query::filtering::Filterable;
/// # use django_query::sorting::Sortable;
/// # use django_query::row::IntoRow;
/// use django_query::mock::{nested_endpoint_matches, NestedEndpoint, NestedEndpointParams};
/// # use std::sync::Arc;
///
/// #[derive(Clone, Filterable, IntoRow, Sortable)]
/// struct Foo {
///   #[django(traverse, foreign_key="id")]
///   bar: Arc<Bar>
/// }
///
/// #[derive(Clone, Filterable, IntoRow, Sortable)]
/// struct Bar {
///   id: i32
/// }
///
/// # tokio_test::block_on( async {
/// let s = wiremock::MockServer::start().await;
///
/// let foos = vec![Foo { bar: Arc::new( Bar { id: 1 }) }];
///
/// wiremock::Mock::given(wiremock::matchers::method("GET"))
///    .and(nested_endpoint_matches("/api", "bars", "foos"))
///    .respond_with(NestedEndpoint::new(
///        foos,
///        NestedEndpointParams {
///            root: "/api/",
///            parent: "bars",
///            child: "foos",
///            parent_query: "bar__id",
///            base_uri: Some(&s.uri())
///        }
///    ))
///    .mount(&s)
///    .await;
///
/// let u = format!("{}/api/bars/1/foos/", s.uri());
/// let body: serde_json::Value = reqwest::get(&u)
///     .await
///     .expect("error getting response")
///     .json()
///     .await
///     .expect("error parsing response");
///
/// assert_eq!(body, serde_json::json!{
///   {
///     "count": 1,
///     "next": null,
///     "previous": null,
///     "results": [
///       { "bar": 1 }
///     ]
///   }
/// });
/// # });
/// ```
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub struct NestedEndpointParams<'a> {
    /// The base mock path, without trailing slash, for example `"/api/v0.2"`
    pub root: &'a str,
    /// The objects this endpoint is nested under, for example `"bars"`.
    pub parent: &'a str,
    /// The objects this endpoint serves, for example `"foos"`
    pub child: &'a str,
    /// The filter path for the parent from the child, so for example
    /// `"parent__id"` if the filter expression for a given `Foo` to
    /// obtain the value used in the query from its `Bar` is
    /// `"parent__id"`.
    pub parent_query: &'a str,
    /// The mock server URI, since wiremock does not make this
    /// available to us; specifying [`None`] here will break
    /// pagination (if requested) until wiremock propagates this
    /// information.
    pub base_uri: Option<&'a str>,
}

impl<'q, T> NestedEndpoint<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
    <T as RowSource>::Item: Send + Sync + Filterable<'q> + Sortable<'q>,
{
    /// Create a new [`NestedEndpoint`].
    ///
    /// The arguments to this function are wrapped in their own
    /// structure: [`NestedEndpointParams`].
    pub fn new(row_source: T, p: NestedEndpointParams<'_>) -> Self {
        Self {
            transform: UrlTransform::new(
                &format!(r"^{}/{}/(?P<parent>[^/]+)/{}/$", p.root, p.parent, p.child),
                &format!("{}/{}/", p.root, p.child),
                vec![(&*p.parent_query, "${parent}")],
            ),
            row_source,
            base_uri: p.base_uri.map(|x| Url::parse(x).unwrap()),
            default_limit: None,
        }
    }

    /// Set the default number of results returned
    ///
    /// This is the value used when no limit is specified in the
    /// query.  This value is configurable in Django; the default
    /// behaviour of this mock endpoint is to return everything, but
    /// that makes testing code that uses the default pagination more
    /// difficult.
    pub fn default_limit(&mut self, limit: usize) {
        self.default_limit = Some(limit);
    }
}

impl<'q, T> Respond for NestedEndpoint<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
    // Note the 'q bound is a consequence of Sortable's implementation for StackedSorter in OrderingSet
    <T as RowSource>::Item: Send + Sync + Filterable<'q> + Sortable<'q> + IntoRow<'q> + 'q,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        trace!("Request URL: {}", request.url);
        let input_url = self.transform.transform(&request.url);
        trace!("Transformed URL: {}", input_url);

        let data = self.row_source.get();
        let mut output_url = request.url.clone();
        if let Some(ref base) = self.base_uri {
            output_url.set_host(base.host_str()).unwrap();
            output_url.set_scheme(base.scheme()).unwrap();
            output_url.set_port(base.port()).unwrap();
        }
        let body = parse_query::<_, _>(
            &input_url,
            &output_url,
            data.get().into_iter(),
            self.default_limit,
        );
        match body {
            Ok(rs) => ResponseTemplate::new(200).set_body_json(rs.mock_json()),
            Err(e) => {
                debug!("Failed to respond to {}: {}", request.url, e);
                ResponseTemplate::new(500).set_body_string(e.to_string())
            }
        }
    }
}

//// Context support

impl<'q, T> ResponseSet<&T> {
    pub fn mock_json_with_context<A>(&self, access: A) -> serde_json::Value
    where
        T: IntoRowWithContext<'q, A>,
        A: 'q,
    {
        let mut map = serde_json::map::Map::new();
        map.insert(
            "count".to_string(),
            serde_json::Value::Number(serde_json::Number::from(self.total_matches)),
        );
        map.insert(
            "results".to_string(),
            to_rows_with_context(&self.contents, access),
        );
        map.insert(
            "next".to_string(),
            self.next
                .as_ref()
                .map(|x| serde_json::Value::String(x.clone()))
                .unwrap_or(serde_json::Value::Null),
        );
        map.insert(
            "previous".to_string(),
            self.prev
                .as_ref()
                .map(|x| serde_json::Value::String(x.clone()))
                .unwrap_or(serde_json::Value::Null),
        );

        serde_json::Value::Object(map)
    }
}

fn to_rows_with_context<'q, T: IntoRowWithContext<'q, A>, A: 'q>(
    data: &[&T],
    access: A,
) -> serde_json::Value {
    let mut array = Vec::new();
    let ser = T::get_serializer(access);
    for item in data {
        array.push(ser.to_json(item));
    }
    serde_json::Value::Array(array)
}

fn parse_query_with_context<'a, 'q, R, I, A>(
    input_url: &Url,
    output_url: &Url,
    access: A,
    iter: I,
    default_limit: Option<usize>,
) -> Result<ResponseSet<&'a R>, MockError>
where
    // Note the 'q bound is a consequence of Sortable's implementation for StackedSorter in OrderingSet
    R: FilterableWithContext<'q, A> + SortableWithContext<'q, A> + IntoRowWithContext<'q, A> + 'q,
    I: Iterator<Item = &'a R>,
    A: Clone + 'q,
{
    let mut rb = ResponseSetBuilder::new(default_limit);
    let qr = OperatorSetWithContext::<R, A>::new(access.clone());
    let sr = OrderingSetWithContext::<R, A>::new(access);
    let pairs = input_url.query_pairs();
    for (key, value) in pairs {
        match key.as_ref() {
            "ordering" => {
                rb.order_by(sr.create_sort(&*value)?);
            }
            "offset" => {
                let v = usize::from_str(value.as_ref())?;
                rb.offset(v);
            }
            "limit" => {
                let v = usize::from_str(value.as_ref())?;
                rb.limit(v);
            }
            _ => {
                if let Ok(filter) = qr.create_filter_from_query_pair(&key, &value) {
                    rb.filter_by(filter);
                    continue;
                }
                return Err(MockError::UnknownQueryParameter(String::from(key.as_ref())));
            }
        }
    }
    let response = rb.apply(iter);
    Ok(ResponseSet::new(
        response.data,
        response.total,
        response
            .next
            .map(|page| make_page_url(output_url, page.offset, page.limit).to_string()),
        response
            .prev
            .map(|page| make_page_url(output_url, page.offset, page.limit).to_string()),
    ))
}

/// A Django-style endpoint for a collection of objects that require a
/// context value.
///
/// [`EndpointWithContext`] implements
/// [`wiremock::Respond`] and so can be mounted directly into a
/// [`wiremock::MockServer`]. It contains a [`RowSource`] which it
/// will query for a fresh data on each query.
///
/// The primary distinction between this type and [`Endpoint`] is that
/// it can be used with types that require a context object, and so
/// relies on the extended traits [`FilterableWithContext`],
/// [`IntoRowWithContext`] and [`SortableWithContext`].
///
/// Any supplied [`RowSource`] needs to generate [`RowSet`] values
/// that double as the context value required by these traits.
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub struct EndpointWithContext<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
{
    row_source: T,
    base_uri: Option<Url>,
    default_limit: Option<usize>,
}

impl<'q, T> EndpointWithContext<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
    // <T as RowSource>::Item: Send + Sync + FilterableWithContext<'q> + SortableWithContext<'q> ,
{
    /// Create a new [`EndpointWithContext`].
    ///
    /// `row_source` is where the data comes from. `base_uri`, if
    /// provided, should be the wiremock server URI.
    ///
    /// `base_uri` is only required because wiremock mangles the
    /// request URL such that it's not possible to inspect it to see
    /// how to give out other URLs on the same server; the port
    /// number, which is usually random when mocking, is lost. Since
    /// Django includes full absolute URLs for the next and preceding
    /// pages for a query, mimicking this is impossible without more
    /// information (i.e. including usable URLs for the next page
    /// within the query response is impossible).
    pub fn new(row_source: T, base_uri: Option<&str>) -> Self {
        Self {
            row_source,
            base_uri: base_uri.map(|x| Url::parse(x).unwrap()),
            default_limit: None,
        }
    }

    /// Set the default number of results returned
    ///
    /// This is the value used when no limit is specified in the
    /// query.  This value is configurable in Django; the default
    /// behaviour of this mock endpoint is to return everything, but
    /// that makes testing code that uses the default pagination more
    /// difficult.
    pub fn default_limit(&mut self, limit: usize) {
        self.default_limit = Some(limit);
    }
}

impl<'q, T, A, R> Respond for EndpointWithContext<T>
where
    T: Send + Sync + RowSource<Rows = A, Item = R>,
    for<'t> &'t <A as RowSet>::Target: IntoIterator<Item = &'t R>,
    // Note the 'q bound is a consequence of Sortable's implementation for StackedSorter in OrderingSet
    R: Send
        + Sync
        + FilterableWithContext<'q, A>
        + SortableWithContext<'q, A>
        + IntoRowWithContext<'q, A>
        + 'q,
    A: Clone + 'q + RowSet,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        // All of this is to work around the mess in wiremock that means
        // request.uri is inaccurate, and can't be used.
        let mut u = request.url.clone();
        if let Some(ref base) = self.base_uri {
            u.set_host(base.host_str()).unwrap();
            u.set_scheme(base.scheme()).unwrap();
            u.set_port(base.port()).unwrap();
        }
        let res = {
            let access = self.row_source.get();
            let res = {
                let into_iter = access.clone();
                let rows = into_iter.get().into_iter();
                let body = parse_query_with_context(
                    &request.url,
                    &u,
                    access.clone(),
                    rows,
                    self.default_limit,
                );
                match body {
                    Ok(rs) => {
                        let bb = rs.mock_json_with_context(access);
                        ResponseTemplate::new(200).set_body_json(bb)
                    }
                    Err(e) => {
                        debug!("Failed to respond to {}: {}", request.url, e);
                        ResponseTemplate::new(500).set_body_string(e.to_string())
                    }
                }
            };
            res
        };
        res
    }
}

/// A Django nested route for types that need a context value.
///
/// A nested route in Django is a way of specifying that a particular
/// filter field must be present when querying for a particular object
/// type, and that the filter type must be exact match. So for
/// example, if all objects of type `Foo` have a reference to a `Bar`,
/// and its too expensive or undesirable to query for `Foo`s without
/// specifying which `Bar` they come from, then you can express this
/// in Django as a nested route:
///
///   ``http://example.com/bars/1/foos``
///
/// where in order to ask any question about `Foo`s you must first
/// identify a particular associated `Bar`.
///
/// Since the django-query crate operates on query parameters, the
/// simplest way to handle this is to convert any relevant parts of
/// the path into query parameters as required, which is all this type
/// does. See its construction parameters [`NestedEndpointParams`] for
/// the details. This type is otherwise identical to
/// [`EndpointWithContext`].
#[cfg_attr(docsrs, doc(cfg(feature = "wiremock")))]
pub struct NestedEndpointWithContext<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
{
    transform: UrlTransform,
    row_source: T,
    base_uri: Option<Url>,
    default_limit: Option<usize>,
}

impl<'q, T> NestedEndpointWithContext<T>
where
    T: Send + Sync + RowSource,
    for<'t> &'t <<T as RowSource>::Rows as RowSet>::Target:
        IntoIterator<Item = &'t <T as RowSource>::Item>,
    // <T as RowSource>::Item: Send + Sync + FilterableWithContext<'q> + SortableWithContext<'q> ,
{
    /// Create a new [`NestedEndpointWithContext`].
    ///
    /// The arguments to this function are wrapped in their own
    /// structure: [`NestedEndpointParams`].
    pub fn new(row_source: T, p: NestedEndpointParams<'_>) -> Self {
        Self {
            transform: UrlTransform::new(
                &format!(r"^{}/{}/(?P<parent>[^/]+)/{}/$", p.root, p.parent, p.child),
                &format!("{}/{}/", p.root, p.child),
                vec![(&*p.parent_query, "${parent}")],
            ),
            row_source,
            base_uri: p.base_uri.map(|x| Url::parse(x).unwrap()),
            default_limit: None,
        }
    }

    pub fn default_limit(&mut self, limit: usize) {
        self.default_limit = Some(limit);
    }
}

impl<'q, T, A, R> Respond for NestedEndpointWithContext<T>
where
    T: Send + Sync + RowSource<Rows = A, Item = R>,
    for<'t> &'t <A as RowSet>::Target: IntoIterator<Item = &'t R>,
    // Note the 'q bound is a consequence of Sortable's implementation for StackedSorter in OrderingSet
    R: Send
        + Sync
        + FilterableWithContext<'q, A>
        + SortableWithContext<'q, A>
        + IntoRowWithContext<'q, A>
        + 'q,
    A: Clone + 'q + RowSet,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        trace!("Request URL: {}", request.url);
        let input_url = self.transform.transform(&request.url);
        trace!("Transformed URL: {}", input_url);

        let mut output_url = request.url.clone();
        if let Some(ref base) = self.base_uri {
            output_url.set_host(base.host_str()).unwrap();
            output_url.set_scheme(base.scheme()).unwrap();
            output_url.set_port(base.port()).unwrap();
        }

        let res = {
            let access = self.row_source.get();
            let res = {
                let into_iter = access.clone();
                let rows = into_iter.get().into_iter();
                let body = parse_query_with_context(
                    &input_url,
                    &output_url,
                    access.clone(),
                    rows,
                    self.default_limit,
                );
                match body {
                    Ok(rs) => {
                        let bb = rs.mock_json_with_context(access);
                        ResponseTemplate::new(200).set_body_json(bb)
                    }
                    Err(e) => {
                        debug!("Failed to respond to {}: {}", request.url, e);
                        ResponseTemplate::new(500).set_body_string(e.to_string())
                    }
                }
            };
            res
        };
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_log::test;
    use wiremock::http::Url;

    #[test]
    fn test_transform() {
        let u = Url::parse("http://foo.bar/jobs/3235/tests/?name=womble&path=bongle")
            .expect("failed to parse url");
        let t = UrlTransform::new(r"^/jobs/(\d+)/tests/$", r"/tests/$1", vec![("job", "$1")]);
        let v = t.transform(&u);
        assert_eq!(
            v,
            Url::parse("http://foo.bar/tests/3235?name=womble&path=bongle&job=3235")
                .expect("failed to parse url")
        );

        let u = Url::parse("http://foo.bar/jobs/hello/tests/?name=womble&path=bongle")
            .expect("failed to parse url");
        let v = t.transform(&u);
        assert_eq!(v, u);

        let u = Url::parse(
            "http://foo.bar/jobs/snomble/bomble/tests/sniffle_snaffle?name=womble&path=bongle",
        )
        .expect("failed to parse url");
        let t = UrlTransform::new(
            r"^/jobs/(?P<name>.*)/tests/(?P<womble>.*)$",
            r"/tests/${name}",
            vec![("job", "$womble")],
        );
        let v = t.transform(&u);
        assert_eq!(
            v,
            Url::parse(
                "http://foo.bar/tests/snomble/bomble?name=womble&path=bongle&job=sniffle_snaffle"
            )
            .expect("failed to parse url")
        );

        let u = Url::parse("http://foo.bar/jobs/hello/tests/?name=womble&path=bongle")
            .expect("failed to parse url");
        let v = t.transform(&u);
        assert_eq!(
            v,
            Url::parse("http://foo.bar/tests/hello?name=womble&path=bongle&job=")
                .expect("failed to parse url")
        );
    }
}
