#![cfg(feature = "mock")]

use core::cmp::min;
use core::fmt::Debug;
use core::ops::Deref;
use std::num::ParseIntError;
use std::str::FromStr;

use log::{debug, trace};
use thiserror::Error;
use wiremock::http::Url;
use wiremock::{Request, Respond, ResponseTemplate};

use crate::{IntoRow, Queryable, QueryableRecord, Sortable, SortableRecord};

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

pub struct ResponseSet<T> {
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

pub fn make_page_url(url: &Url, offset: usize, limit: usize) -> Url {
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

pub trait RowSource
where
    Self::Rows: Deref,
    for<'a> &'a <Self::Rows as Deref>::Target: IntoIterator<Item = &'a Self::Item>,
{
    type Item;
    type Rows;
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

pub struct Endpoint<T> {
    row_source: T,
}

impl<T, R> Endpoint<T>
where
    T: Send + Sync + RowSource<Item = R>,
    R: Queryable + Sortable + 'static,
    <T as RowSource>::Rows: Deref,
    for<'a> &'a <<T as RowSource>::Rows as Deref>::Target: IntoIterator<Item = &'a R>,
{
    pub fn new(row_source: T) -> Self {
        Self { row_source }
    }

    fn parse_query<'a>(
        url: &Url,
        iter: <&'a <<T as RowSource>::Rows as Deref>::Target as IntoIterator>::IntoIter,
    ) -> Result<ResponseSet<&'a R>, MockError> {
        let mut rb = ResponseSetBuilder::new();
        let qr = QueryableRecord::<R>::new();
        let sr = SortableRecord::<R>::new();
        let pairs = url.query_pairs();
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
}

impl<T, R> Respond for Endpoint<T>
where
    T: Send + Sync + RowSource<Item = R>,
    R: Queryable + Sortable + IntoRow + 'static,
    <T as RowSource>::Rows: Deref,
    for<'a> &'a <<T as RowSource>::Rows as Deref>::Target: IntoIterator<Item = &'a R>,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        trace!("Request URL: {}", request.url);
        let data: <T as RowSource>::Rows = self.row_source.get();
        let body = Self::parse_query(&request.url, (&data).into_iter());
        match body {
            Ok(rs) => ResponseTemplate::new(200).set_body_json(rs.mock_json()),
            Err(e) => {
                debug!("Failed to respond to {}: {}", request.url, e);
                ResponseTemplate::new(500).set_body_string(e.to_string())
            }
        }
    }
}
