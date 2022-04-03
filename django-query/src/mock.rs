#![cfg(feature = "mock")]
//! Create Django-style endpoints using [wiremock].
//!
//! This module provides [Endpoint] which can function as an endpoint
//! in wiremock (it implements [Respond]). It will dissect any
//! URLs given to it, pulling out:
//! - pagination requests (`limit`, `offset`) which it handles itself.
//! - sort requests (`ordering`) which it uses [OrderingSet] to parse.
//! - filtering requests which it uses [OperatorSet] to parse.
//!
//! Finally, it uses [IntoRow] to generate the response table as JSON,
//! in a similar way to Django.
//!
//! Example
//! ```rust
//! use django_query::{IntoRow, Filterable, Sortable, mock::Endpoint};
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
//! tokio_test::block_on( async {
//!    let server = MockServer::start().await;
//!
//!    Mock::given(matchers::method("GET"))
//!         .respond_with(Endpoint::new(Arc::new(foos), Some(&server.uri())))
//!         .mount(&server)
//!         .await;
//!
//!    let u = format!("{}?limit=1&offset=5&a__lt=10&ordering=-a", server.uri());
//!    let body: serde_json::Value = reqwest::get(&u)
//!        .await
//!        .expect("error getting response")
//!        .json()
//!        .await
//!        .expect("error parsing response");
//!
//!    let prev = format!("{}/?limit=1&offset=4&a__lt=10&ordering=-a", server.uri());
//!    let next = format!("{}/?limit=1&offset=6&a__lt=10&ordering=-a", server.uri());
//!    assert_eq!(body, serde_json::json!{
//!      {
//!        "count": 1,
//!        "next": next,
//!        "prev": prev,
//!        "results": [
//!          { "a": 4 }
//!        ]
//!      }
//!    });
//! });

//! ```
use core::cmp::min;
use core::fmt::Debug;
use core::ops::Deref;
use regex::{Captures, Regex};
use std::num::ParseIntError;
use std::str::FromStr;

use log::{debug, trace};
use thiserror::Error;
use wiremock::http::Url;
use wiremock::{Request, Respond, ResponseTemplate};

use crate::{Filterable, IntoRow, OperatorSet, OrderingSet, Sortable};

use crate::filtering::Filter;
use crate::ordering::Sorter;

#[derive(Debug, Error)]
pub enum MockError {
    #[error("unknown query parameter `{0}`")]
    UnknownQueryParameter(String),
    #[error("expected integer value in query")]
    BadIntegerInQuery(#[from] ParseIntError),
    #[error("bad value in query: {0}")]
    BadValue(#[from] strum::ParseError),
    #[error("bad datetime value in query")]
    BadDateInQuery(#[from] chrono::ParseError),
    #[error("invalid sort: {0}")]
    BadSort(#[from] crate::ordering::SorterError),
}

struct ResponseSet<T> {
    contents: Vec<T>,
    next: Option<String>,
    prev: Option<String>,
}

impl<T> ResponseSet<T> {
    pub fn new(contents: Vec<T>, next: Option<String>, prev: Option<String>) -> Self {
        ResponseSet {
            contents,
            next,
            prev,
        }
    }
}

impl<T: IntoRow> ResponseSet<&T> {
    pub fn mock_json(&self) -> serde_json::Value {
        let mut map = serde_json::map::Map::new();
        map.insert(
            "count".to_string(),
            serde_json::Value::Number(serde_json::Number::from(self.contents.len())),
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
            "prev".to_string(),
            self.prev
                .as_ref()
                .map(|x| serde_json::Value::String(x.clone()))
                .unwrap_or(serde_json::Value::Null),
        );

        serde_json::Value::Object(map)
    }
}

fn to_rows<T: IntoRow>(data: &[&T]) -> serde_json::Value {
    let mut array = Vec::new();
    for item in data {
        array.push(item.to_json());
    }
    serde_json::Value::Array(array)
}

struct Page {
    offset: usize,
    limit: usize,
}

struct PaginatedResponse<'a, T> {
    data: Vec<&'a T>,
    next: Option<Page>,
    prev: Option<Page>,
}

struct ResponseSetBuilder<T> {
    ordering: Vec<Box<dyn Sorter<T>>>,
    filtering: Vec<Box<dyn Filter<T>>>,
    limit: Option<usize>,
    offset: usize,
}

impl<T> Default for ResponseSetBuilder<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> ResponseSetBuilder<T> {
    pub fn new() -> Self {
        ResponseSetBuilder {
            ordering: Vec::new(),
            filtering: Vec::new(),
            limit: None,
            offset: 0,
        }
    }

    pub fn order_by(&mut self, order: Box<dyn Sorter<T>>) -> &mut Self {
        self.ordering.push(order);
        self
    }

    pub fn filter_by(&mut self, filter: Box<dyn Filter<T>>) -> &mut Self {
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

    pub fn apply<'a, I: Iterator<Item = &'a T>>(&mut self, iter: I) -> PaginatedResponse<'a, T> {
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
            next,
            prev,
        }
    }
}

fn make_page_url(url: &Url, offset: usize, limit: usize) -> Url {
    let mut new_url = url.clone();

    let mut new_pairs = new_url.query_pairs_mut();
    let old_pairs = url.query_pairs();

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
            }
            _ => {
                new_pairs.append_pair(key.as_ref(), value.as_ref());
            }
        }
    }
    new_pairs.finish();
    drop(new_pairs);

    new_url
}

/// Something which can provide a collection of objects on demand.
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
/// using `Arc<Vec<T>>` as a `RowSource` does not entail copying data.
/// It's possible to define mutable types that generate snapshots on
/// demand which also implement `RowSource`, if you need your test
/// data to evolve.
///
/// The main drawback of this definition is that supporting bare
/// containers becomes very expensive. `Vec<T>` is defined to be a
/// `RowSource`, but the entire [Vec] must be copied for every call to
/// [get](RowSource::get).
pub trait RowSource
where
    Self::Rows: Deref,
    for<'a> &'a <Self::Rows as Deref>::Target: IntoIterator<Item = &'a Self::Item>,
{
    /// The type of the objects this provides.
    type Item;
    /// The type of the collection this provides.
    type Rows;
    /// Return a new, owned collection of objects, which should now
    /// remain immutable.
    fn get(&self) -> Self::Rows;
}

impl<T: Clone> RowSource for Vec<T> {
    type Item = T;
    type Rows = Self;
    fn get(&self) -> Self::Rows {
        self.clone()
    }
}

impl<T> RowSource for std::sync::Arc<Vec<T>> {
    type Item = T;
    type Rows = Self;
    fn get(&self) -> Self::Rows {
        self.clone()
    }
}

fn parse_query<'a, T, R>(
    url: &Url,
    iter: <&'a <<T as RowSource>::Rows as Deref>::Target as IntoIterator>::IntoIter,
) -> Result<ResponseSet<&'a R>, MockError>
where
    T: Send + Sync + RowSource<Item = R>,
    R: Filterable + Sortable + IntoRow + 'static,
    <T as RowSource>::Rows: Deref,
    for<'t> &'t <<T as RowSource>::Rows as Deref>::Target: IntoIterator<Item = &'t R>,
{
    println!("Parse query");
    let mut rb = ResponseSetBuilder::new();
    let qr = OperatorSet::<R>::new();
    let sr = OrderingSet::<R>::new();
    let pairs = url.query_pairs();
    println!("Getting pairs from url {:?} - {}", url, url);
    for (key, value) in pairs {
        println!("Processing qp {}={}", key, value);
        match key.as_ref() {
            "ordering" => {
                rb.order_by(sr.create_sort(&*value)?);
            }
            "offset" => {
                println!("Saw offset {}", value.as_ref());
                let v = usize::from_str(value.as_ref())?;
                rb.offset(v);
            }
            "limit" => {
                println!("Saw limit {}", value.as_ref());
                let v = usize::from_str(value.as_ref())?;
                rb.limit(v);
            }
            _ => match qr.create_filter_from_query_pair(&key, &value) {
                Ok(filter) => {
                    rb.filter_by(filter);
                }
                Err(_) => {
                    return Err(MockError::UnknownQueryParameter(String::from(key.as_ref())));
                }
            },
        }
    }
    let response = rb.apply(iter);
    Ok(ResponseSet::new(
        response.data,
        response
            .next
            .map(|page| make_page_url(url, page.offset, page.limit).to_string()),
        response
            .prev
            .map(|page| make_page_url(url, page.offset, page.limit).to_string()),
    ))
}

/// A Django-style Wiremock endpoint for a collection of suitable objects.
///
/// This is the central struct in this crate. `Endpoint` implements
/// [Respond](wiremock::Respond and so can be mounted directly into a
/// [MockServer](wiremock::MockServer). It contains a [RowSource]
/// which it will query for a starting set of data.
///
/// It will then filter that data using the URL, using the
/// [Filterable] trait on the objects; then it will sort the data
/// using the [Sortable] trait on the objects; then it will clip the
/// results to the selected page using `limit` and `offset`;finally it
/// will convert the data to JSON using the [IntoRow] trait on the
/// objects.
pub struct Endpoint<T> {
    row_source: T,
    base_uri: Option<Url>,
}

impl<T, R> Endpoint<T>
where
    T: Send + Sync + RowSource<Item = R>,
    R: Filterable + Sortable + 'static,
    <T as RowSource>::Rows: Deref,
    for<'a> &'a <<T as RowSource>::Rows as Deref>::Target: IntoIterator<Item = &'a R>,
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
        }
    }
}

impl<T, R> Respond for Endpoint<T>
where
    T: Send + Sync + RowSource<Item = R>,
    R: Filterable + Sortable + IntoRow + 'static,
    <T as RowSource>::Rows: Deref,
    for<'a> &'a <<T as RowSource>::Rows as Deref>::Target: IntoIterator<Item = &'a R>,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        let data: <T as RowSource>::Rows = self.row_source.get();
        // All of this is to work around the mess in wiremock that means
        // request.uri is inaccurate, and can't be used.
        let mut u = request.url.clone();
        if let Some(ref base) = self.base_uri {
            u.set_host(base.host_str()).unwrap();
            u.set_scheme(base.scheme()).unwrap();
            u.set_port(base.port()).unwrap();
        }
        let body = parse_query::<T, R>(&u, (&data).into_iter());
        match body {
            Ok(rs) => ResponseTemplate::new(200).set_body_json(rs.mock_json()),
            Err(e) => {
                debug!("Failed to respond to {}: {}", request.url, e);
                ResponseTemplate::new(500).set_body_string(e.to_string())
            }
        }
    }
}

fn replace_into(target: &str, cap: &Captures<'_>) -> String {
    let mut res = String::new();
    cap.expand(target, &mut res);
    res
}

#[allow(rustdoc::bare_urls)]
/// Use a regex to match a query path, mutating both the path and query string.
///
/// Some Django-based services support nested endpoints, where the
/// path of the URL might look like:
///
///    <http://example.com/womble/6/neighbours>
///
/// Since our parsing operates on the query parameters, we'd like to rewrite this
/// URL before we process it, to fit the remainder of the pipeline better. For
/// example we might want to create:
///
///    <http://example.com/neighbours?womble=6>
///
/// from the preceding URL.
///
/// Example:
/// ```rust
/// use wiremock::http::Url;
/// use django_query::mock::UrlTransform;
///
/// let ut = UrlTransform::new(r"^/base/foos/(\d+)/(.+)$",
///                            r"/base/$2",
///                            vec![("foo", "$1")]);
/// let u = Url::parse("http://example.com/base/foos/1214/bars?ordering=name").unwrap();
/// let t = Url::parse("http://example.com/base/bars?ordering=name&foo=1214").unwrap();
/// assert_eq!(ut.transform(&u), t);
/// ```
///
/// This is intended for use with [NestedEndpoint], which differs from
/// [Endpoint] only in applying a [UrlTransform] to its input.
pub struct UrlTransform {
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
/// A nested route in Django is a way of specifying that a particular filter field must
/// be present when querying for a particular object type, and that it the filter type
/// must be exact match. So for example, if all objects of type `Foo` have a reference to
/// a `Bar`, and its too expensive or undesirable to query for `Foo`s without specifying
/// which `Bar` they come from, then you can express this in Django as a nested route:
///
///   ``http://example.com/bars/my-bar-id/foos``
///
/// where in order to ask any question about `Foo`s you must first
/// identify a particular associated `Bar`.
///
/// Since the django-query crate operates on query parameters, the
/// simplest way to handle this is to use a [UrlTransform] to convert
/// any relevant parts of the path into query parameters as required,
/// which is all this type does.  It is otherwise identical to
/// [Endpoint].
pub struct NestedEndpoint<T> {
    transform: UrlTransform,
    row_source: T,
}

impl<T, R> NestedEndpoint<T>
where
    T: Send + Sync + RowSource<Item = R>,
    R: Filterable + Sortable + 'static,
    <T as RowSource>::Rows: Deref,
    for<'a> &'a <<T as RowSource>::Rows as Deref>::Target: IntoIterator<Item = &'a R>,
{
    /// Create a new NestedEndpoint.
    ///
    /// The given [UrlTransform] will be applied to every incoming
    /// request. The `row_source` will be used to gather the data to
    /// respond to the rewritten request.
    pub fn new(transform: UrlTransform, row_source: T) -> Self {
        Self {
            transform,
            row_source,
        }
    }
}

impl<T, R> Respond for NestedEndpoint<T>
where
    T: Send + Sync + RowSource<Item = R>,
    R: Filterable + Sortable + IntoRow + 'static,
    <T as RowSource>::Rows: Deref,
    for<'a> &'a <<T as RowSource>::Rows as Deref>::Target: IntoIterator<Item = &'a R>,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        trace!("Request URL: {}", request.url);
        let u = self.transform.transform(&request.url);
        trace!("Transformed URL: {}", u);

        let data: <T as RowSource>::Rows = self.row_source.get();
        let body = parse_query::<T, R>(&u, (&data).into_iter());
        match body {
            Ok(rs) => ResponseTemplate::new(200).set_body_json(rs.mock_json()),
            Err(e) => {
                debug!("Failed to respond to {}: {}", request.url, e);
                ResponseTemplate::new(500).set_body_string(e.to_string())
            }
        }
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
