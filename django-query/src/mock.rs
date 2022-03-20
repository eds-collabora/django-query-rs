#![cfg(feature = "mock")]

use core::cmp::{max, min, Ordering};
use core::fmt::Debug;
use std::num::ParseIntError;
use std::str::FromStr;

use thiserror::Error;
use wiremock::http::Url;
use wiremock::{Request, Respond, ResponseTemplate};

use crate::{IntoRow, QueryableRecord, SortableRecord, Queryable, Sortable};

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

fn to_rows<T: IntoRow>(data: &Vec<&T>) -> serde_json::Value {
    let mut array = Vec::new();
    for item in data {
        array.push(item.to_json());
    }
    serde_json::Value::Array(array)
}

pub struct ResponseSetBuilder<T> {
    ordering: Vec<Box<dyn FnMut(&T, &T) -> Ordering>>,
    filtering: Vec<Box<dyn FnMut(&T) -> bool>>,
    limit: usize,
    offset: usize,
}

impl<T: std::fmt::Debug> ResponseSetBuilder<T> {
    pub fn new() -> Self {
        ResponseSetBuilder {
            ordering: Vec::new(),
            filtering: Vec::new(),
            limit: 0,
            offset: 0,
        }
    }

    pub fn order_by<Op: 'static + FnMut(&T, &T) -> Ordering>(&mut self, order: Op) -> &mut Self {
        self.ordering.push(Box::new(order));
        self
    }

    pub fn filter_by<Op: 'static + FnMut(&T) -> bool>(&mut self, filter: Op) -> &mut Self {
        self.filtering.push(Box::new(filter));
        self
    }

    pub fn limit(&mut self, limit: usize) -> &mut Self {
        self.limit = limit;
        self
    }

    pub fn offset(&mut self, offset: usize) -> &mut Self {
        self.offset = offset;
        self
    }

    pub fn apply<'a, I: Iterator<Item = &'a T>>(
        &mut self,
        iter: I,
    ) -> (Vec<&'a T>, Option<(usize, usize)>, Option<(usize, usize)>) {
        let mut v = Vec::new();
        for item in iter {
            let mut rejected = false;
            for f in &mut self.filtering {
                if !f(item) {
                    rejected = true;
                    break;
                }
            }
            if !rejected {
                v.push(item);
            }
        }

        for order in &mut self.ordering {
            v.sort_by(|&x, &y| order(x, y));
        }

        let start = min(v.len(), self.offset);
        let prev = if start > 0 {
            Some((max(0, self.offset - self.limit), self.limit))
        } else {
            None
        };

        v.drain(..start);

        let end = max(v.len(), self.limit);
        let next = if end < v.len() {
            Some((self.offset + self.limit, self.limit))
        } else {
            None
        };

        v.drain(end..);

        (v, next, prev)
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

pub trait RowSource<'a>
where
    Self::Rows: IntoIterator<Item=Self::Item>,
{
    type Item;
    type Rows;
    fn get(&'a self) -> Self::Rows;
}

impl<'a, T: 'a> RowSource<'a> for Vec<T> {
    type Item = &'a T;
    type Rows = &'a Vec<T>;
    fn get(&'a self) -> Self::Rows {
        self
    }
}

pub struct Endpoint<T> {
    rows: T
}

impl<T, R> Endpoint<T>
where
    T: Send + Sync,
    R: Queryable + Sortable + Debug + 'static,
    for<'t> &'t T: IntoIterator<Item=&'t R>
{
    pub fn new(rows: T) -> Self {
        Self { rows }
    }

    fn parse_query<'a>(
        url: &Url,
        iter: <&'a T as IntoIterator>::IntoIter,
    ) -> Result<ResponseSet<&'a R>, MockError> {
        let mut rb = ResponseSetBuilder::new();
        let qr = QueryableRecord::<R>::new();
        let sr = SortableRecord::<R>::new();
        let pairs = url.query_pairs();
        for (key, value) in pairs {
            match key.as_ref() {
                "ordering" => {
                    let sort = sr.create_sort(&*value)?;
                    rb.order_by(move |x, y| sort.compare(x, y));
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
                        rb.filter_by(move |x| filter.filter_one(x));
                    }
                    Err(_) => {
                        return Err(MockError::UnknownQueryParameter(String::from(key.as_ref())));
                    }
                },
            }
        }
        let (data, next, prev) = rb.apply(iter);
        Ok(ResponseSet::new(
            data,
            next.map(|(offset, limit)| make_page_url(url, offset, limit).to_string()),
            prev.map(|(offset, limit)| make_page_url(url, offset, limit).to_string()),
        ))
    }
    
}

impl<T, R> Respond for Endpoint<T>
where
    T: Send + Sync,
    R: Queryable + Sortable + IntoRow + Debug + 'static,
    for<'c> &'c T: IntoIterator<Item = &'c R>,
{
    fn respond(&self, request: &Request) -> ResponseTemplate {
        let body = Self::parse_query(&request.url, self.rows.into_iter());
        match body {
            Ok(rs) => ResponseTemplate::new(200).set_body_json(rs.mock_json()),
            Err(e) => ResponseTemplate::new(500).set_body_string(e.to_string()),
        }
    }
}