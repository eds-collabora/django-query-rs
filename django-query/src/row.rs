//! Convert Rust structs with named fields into tables of output.
//!
//! Django output pagination, filtering, and ordering is dealt with
//! separately. This module is concerned with producing the individual
//! formatted items in a Django query result, once those have been
//! determined.
//!
//! We are mimicking what is essentially a database query result
//! encoded into JSON, with each row represented by an object, and
//! each cell in that row being the value of a member of this
//! object. The column names in this encoding are the names of the
//! members of each of the row objects.
//!
//! The [IntoRow] trait in this module is for converting an instance
//! into a row within a table of output. The derive macro for this
//! trait is an easy way to get an implementation for structs with
//! named fields. Note that the values of the cells in a row cannot
//! themselves be objects (this would effectively be a nested table),
//! but they can be arrays. The [CellValue] type encodes this
//! restriction.
//!
//! A type which can be value of a particular column in a particular
//! row should implement [IntoCellValue]. If the type implements
//! [Display] and the desired JSON representation is a string, it's
//! simplest to derive the marker trait [StringCellValue] to benefit
//! from a blanket derivation of [IntoCellValue]. Otherwise the trait
//! must be implemented directly.
//!
//! The concept of foreign keys in this module is a direct carryover
//! from the database model we are emulating. Django results do not
//! contain nested objects, instead one field of the nested object is
//! chosen to represent the object - a foreign key. The [AsForeignKey]
//! trait captures this idea.
//!
//! Example:
//! ```rust
//! use django_query::IntoRow;
//! use serde_json::json;
//! use std::sync::Arc;
//!
//! #[derive(IntoRow)]
//! struct Foo {
//!   a: i32,
//!   #[django(foreign_key="a")]
//!   b: Option<Arc<Foo>>
//! }
//!
//! let f1 = Arc::new(Foo { a: 1, b: None });
//! let f2 = Arc::new(Foo { a: 2, b: Some(f1.clone()) });
//! let f3 = Arc::new(Foo { a: 3, b: Some(f2.clone()) });
//!
//! assert_eq!(f1.to_json(), json! {
//!   { "a": 1i32, "b": null }
//! });
//! assert_eq!(f2.to_json(), json! {
//!   { "a": 2i32, "b": 1i32 }
//! });
//! assert_eq!(f3.to_json(), json! {
//!   { "a": 3i32, "b": 2i32 }
//! });
//! ```
use std::collections::BTreeMap;
use std::fmt::Display;
use std::sync::Arc;

use chrono::DateTime;
use serde_json::value::Value;
use serde_json::Number;

/// The JSON types Django permits in cells - everything except object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CellValue {
    /// A JSON null
    Null,
    /// A JSON boolean
    Bool(bool),
    /// A JSON number
    Number(Number),
    /// A JSON string
    String(String),
    /// A JSON array of non-object values
    Array(Vec<CellValue>),
}

/// For things that can be converted into cell values within a Django output row.
pub trait IntoCellValue {
    fn to_cell_value(&self) -> CellValue;
}

/// Marker which means use [Display] to convert the type into a [CellValue]
pub trait StringCellValue {}

impl StringCellValue for String {}

impl<T> IntoCellValue for T
where
    T: StringCellValue + Display,
{
    fn to_cell_value(&self) -> CellValue {
        CellValue::String(self.to_string())
    }
}

impl IntoCellValue for i8 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for u8 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for i16 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for u16 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for i32 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for u32 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for i64 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for u64 {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for isize {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for usize {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Number(serde_json::Number::from(*self))
    }
}

impl IntoCellValue for f32 {
    fn to_cell_value(&self) -> CellValue {
        serde_json::Number::from_f64((*self).into())
            .map(CellValue::Number)
            .unwrap_or_else(|| CellValue::Null)
    }
}

impl IntoCellValue for f64 {
    fn to_cell_value(&self) -> CellValue {
        serde_json::Number::from_f64(*self)
            .map(CellValue::Number)
            .unwrap_or_else(|| CellValue::Null)
    }
}

impl IntoCellValue for bool {
    fn to_cell_value(&self) -> CellValue {
        CellValue::Bool(*self)
    }
}

impl<T> IntoCellValue for DateTime<T>
where
    T: chrono::TimeZone,
    <T as chrono::TimeZone>::Offset: Display,
{
    fn to_cell_value(&self) -> CellValue {
        CellValue::String(self.to_rfc3339_opts(chrono::SecondsFormat::Micros, true))
    }
}

impl<T> IntoCellValue for Option<T>
where
    T: IntoCellValue,
{
    fn to_cell_value(&self) -> CellValue {
        if let Some(ref x) = self {
            x.to_cell_value()
        } else {
            CellValue::Null
        }
    }
}

impl<T> IntoCellValue for Vec<T>
where
    T: IntoCellValue,
{
    fn to_cell_value(&self) -> CellValue {
        CellValue::Array(self.iter().map(|item| item.to_cell_value()).collect())
    }
}

/// Something that can visit the values in a Django output row.
pub trait CellVisitor {
    /// Visit a value in the row, where:
    /// - `name` is the name of the column
    /// - `v` is the value in that column for this row
    fn visit_value(&mut self, name: &str, v: CellValue);
}

/// Something that can visit the columns that a Django output row sequence contains.
pub trait ColumnVisitor {
    /// Visit a column in the row where:
    /// - `name` is the name of the column
    fn visit_column(&mut self, name: &str);
}

/// Something that can be converted into a row in Django output.
///
/// This is suitable for the top level of Django output; i.e. things
/// whose collection has its own endpoint.
pub trait IntoRow {
    /// Visit the values in a row of output for this type using
    /// `visitor`.
    fn accept_cell_visitor<V: CellVisitor>(&self, visitor: &mut V);

    /// Visit the columns in a row using `visitor`; note that this
    /// does not require an instance of the type.
    fn accept_column_visitor<V: ColumnVisitor>(visitor: &mut V);

    /// Convert an instance of this type into a BTreeMap.
    fn to_row(&self) -> BTreeMap<String, CellValue> {
        let mut r = RowVisitor {
            values: BTreeMap::new(),
        };
        self.accept_cell_visitor(&mut r);
        r.values
    }

    /// Convert an instance of this type into a [serde_json::Value]
    fn to_json(&self) -> Value {
        let mut j = JsonVisitor {
            value: serde_json::map::Map::new(),
        };
        self.accept_cell_visitor(&mut j);
        Value::Object(j.value)
    }

    /// Collect the columns for a table of this type into an ordered
    /// sequence.
    fn columns() -> Vec<String> {
        let mut c = ColumnListVisitor { value: Vec::new() };
        Self::accept_column_visitor(&mut c);
        c.value
    }
}

impl<T> IntoRow for Option<T>
where
    T: IntoRow,
{
    fn accept_cell_visitor<V: CellVisitor>(&self, visitor: &mut V) {
        if let Some(ref item) = self {
            item.accept_cell_visitor(visitor);
        } else {
            let mut n = NullColumnVisitor { parent: visitor };
            T::accept_column_visitor(&mut n);
        }
    }

    fn accept_column_visitor<V: ColumnVisitor>(visitor: &mut V) {
        T::accept_column_visitor(visitor);
    }
}

impl<T> IntoRow for Arc<T>
where
    T: IntoRow,
{
    fn accept_cell_visitor<V: CellVisitor>(&self, visitor: &mut V) {
        T::accept_cell_visitor(&*self, visitor);
    }

    fn accept_column_visitor<V: ColumnVisitor>(visitor: &mut V) {
        T::accept_column_visitor(visitor);
    }
}

/// Something which can be stored in a cell by specifying one of its
/// fields to stand in for it.
pub trait AsForeignKey {
    /// Return the representation of the object if the column `name`
    /// of its own table is used to stand in for it.
    fn as_foreign_key(&self, name: &str) -> CellValue;
}

impl<T> AsForeignKey for T
where
    T: IntoRow,
{
    fn as_foreign_key(&self, name: &str) -> CellValue {
        let mut k = ForeignKeyVisitor {
            target: name,
            value: None,
        };
        self.accept_cell_visitor(&mut k);
        k.value.unwrap_or(CellValue::Null)
    }
}

impl<T> AsForeignKey for Vec<T>
where
    T: AsForeignKey,
{
    fn as_foreign_key(&self, name: &str) -> CellValue {
        let mut v = Vec::new();
        for item in self.iter() {
            v.push(item.as_foreign_key(name))
        }
        CellValue::Array(v)
    }
}

struct ForeignKeyVisitor<'a> {
    pub target: &'a str,
    pub value: Option<CellValue>,
}

impl<'a> CellVisitor for ForeignKeyVisitor<'a> {
    fn visit_value(&mut self, name: &str, v: CellValue) {
        if name == self.target {
            self.value = Some(v);
        }
    }
}

struct RowVisitor {
    pub values: BTreeMap<String, CellValue>,
}

impl CellVisitor for RowVisitor {
    fn visit_value(&mut self, name: &str, v: CellValue) {
        self.values.insert(name.to_string(), v);
    }
}

impl From<CellValue> for Value {
    fn from(v: CellValue) -> Self {
        match v {
            CellValue::Null => Value::Null,
            CellValue::Bool(b) => Value::Bool(b),
            CellValue::Number(n) => Value::Number(n),
            CellValue::String(s) => Value::String(s),
            CellValue::Array(a) => Value::Array(a.into_iter().map(|x| x.into()).collect()),
        }
    }
}

struct JsonVisitor {
    pub value: serde_json::map::Map<String, Value>,
}

impl CellVisitor for JsonVisitor {
    fn visit_value(&mut self, name: &str, v: CellValue) {
        self.value.insert(name.to_string(), v.into());
    }
}

struct ColumnListVisitor {
    pub value: Vec<String>,
}

impl ColumnVisitor for ColumnListVisitor {
    fn visit_column(&mut self, name: &str) {
        self.value.push(name.to_string())
    }
}

struct NullColumnVisitor<'a, V> {
    parent: &'a mut V,
}

impl<'a, V> ColumnVisitor for NullColumnVisitor<'a, V>
where
    V: CellVisitor,
{
    fn visit_column(&mut self, name: &str) {
        self.parent.visit_value(name, CellValue::Null);
    }
}
