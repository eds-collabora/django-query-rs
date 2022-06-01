//! # Convert Rust structs with named fields into tables of output
//!
//! Django produces JSON encoded tabular output in response to queries,
//! where each row of the output is a JSON object, and those objects do not
//! contain any nested objects. Instead, nested objects are represented
//! by a pre-selected foreign key.
//!
//! # Overview
//!
//! The [`IntoRow`] trait in this module allows the type to produce a [`Serializer`] that
//! can convert an instance into a row within a table of JSON output.
//! The [`macro@IntoRow`] derive macro for this
//! trait will produce everything necessary for structs with
//! named fields.
//!
//! The value of a cell within the table cannot itself be JSON
//! object-typed, in line with Django's database origins. However, it
//! is possible to return multiple values in an array inside one
//! cell. The [`CellValue`] type captures the subset of permitted JSON
//! [`Value`](serde_json::Value)s that are permitted.
//!
//! A type which can directly be the value in a given cell of output
//! should implement [`IntoCellValue`]. If the type implements
//! [`Display`], and if the desired JSON representation is a string,
//! you can derive the marker trait [`StringCellValue`] to benefit
//! from a blanket derivation of [`IntoCellValue`] for string
//! values. Otherwise [`IntoCellValue`] must be implemented directly.
//!
//! Since Django results do not contain nested objects, fields with
//! structured types must be handled differently. Here, one field of
//! the nested type is chosen to represent that value, as what is in
//! effect a foreign key. The [`AsForeignKey`] trait captures this
//! idea, and is blanket implemented for [`IntoRow`] types.
//!
//! Example:
//! ```rust
//! use django_query::row::{Serializer, IntoRow};
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
//! let ser = Arc::<Foo>::get_serializer();
//!
//! assert_eq!(ser.to_json(&f1), json! {
//!   { "a": 1i32, "b": null }
//! });
//! assert_eq!(ser.to_json(&f2), json! {
//!   { "a": 2i32, "b": 1i32 }
//! });
//! assert_eq!(ser.to_json(&f3), json! {
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
    /// A null
    Null,
    /// A Boolean
    Bool(bool),
    /// A number
    Number(Number),
    /// A string
    String(String),
    /// An array of non-object values
    Array(Vec<CellValue>),
}

/// Convert a value into a JSON [`CellValue`].
///
/// [`CellValue`] captures the types that can appear within a cell of
/// the output table.
pub trait IntoCellValue {
    fn to_cell_value(&self) -> CellValue;
}

/// Use [`Display`] to convert the type into a [`CellValue`]
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

/// Visit the values in the Django output row for a given value.
pub trait CellVisitor {
    /// Visit a value in the row, where:
    /// - `name` is the name of the column
    /// - `v` is the value in that column for this row
    fn visit_value(&mut self, name: &str, v: CellValue);
}

/// Visit the columns that a Django output row for a given type will contain.
pub trait ColumnVisitor {
    /// Visit a column in the row where:
    /// - `name` is the name of the column
    fn visit_column(&mut self, name: &str);
}

/// Produce a [`Serializer`] to convert values of this type into rows
/// of output.
pub trait IntoRow<'s>: Sized {
    type Serializer: Serializer<'s, Self>;
    fn get_serializer() -> Self::Serializer;
}

/// A converter that visits rows of Django output for a given type.
///
/// This is suitable for the top level of Django output; i.e. things
/// whose collection has its own endpoint.
pub trait Serializer<'a, T>
where
    Self: 'a,
{
    /// Visit the values in a row of output for this type using
    /// `visitor`.
    fn accept_cell_visitor<V: CellVisitor>(&self, value: &T, visitor: &mut V);

    /// Visit the columns in a row using `visitor`; note that this
    /// does not require an instance of the type.
    fn accept_column_visitor<V: ColumnVisitor>(&self, visitor: &mut V);

    /// Convert an instance of this type into a [`BTreeMap`].
    fn to_row(&self, value: &T) -> BTreeMap<String, CellValue> {
        let mut r = RowVisitor {
            values: BTreeMap::new(),
        };
        self.accept_cell_visitor(value, &mut r);
        r.values
    }

    /// Convert an instance of this type into a [`serde_json::Value`]
    fn to_json(&self, value: &T) -> Value {
        let mut j = JsonVisitor {
            value: serde_json::map::Map::new(),
        };
        self.accept_cell_visitor(value, &mut j);
        Value::Object(j.value)
    }

    /// Collect the columns for a table of this type into an ordered
    /// sequence.
    fn columns(&self) -> Vec<String> {
        let mut c = ColumnListVisitor { value: Vec::new() };
        self.accept_column_visitor(&mut c);
        c.value
    }
}

#[doc(hidden)]
pub trait SelfSerializer<'a>
where
    Self: 'a,
{
    /// Visit the values in a row of output for this type using
    /// `visitor`.
    fn accept_cell_visitor<V: CellVisitor>(&self, visitor: &mut V);

    /// Visit the columns in a row using `visitor`; note that this
    /// does not require an instance of the type.
    fn accept_column_visitor<V: ColumnVisitor>(visitor: &mut V);
}

// Note the presence of the marker is to prevent type annotations
// elsewhere, otherwise even though we have a constraint on some other
// type that the serializer is for Self rust cannot rule out that we
// wanted to use it for some other type.
#[doc(hidden)]
pub struct DefaultSelfSerializer<T> {
    _marker: core::marker::PhantomData<T>,
}

impl<'a, T> IntoRow<'a> for T
where
    T: SelfSerializer<'a>,
{
    type Serializer = DefaultSelfSerializer<Self>;
    fn get_serializer() -> Self::Serializer {
        DefaultSelfSerializer {
            _marker: Default::default(),
        }
    }
}

impl<'a, T> Serializer<'a, T> for DefaultSelfSerializer<T>
where
    T: SelfSerializer<'a>,
{
    fn accept_cell_visitor<V: CellVisitor>(&self, value: &T, visitor: &mut V) {
        value.accept_cell_visitor(visitor)
    }

    fn accept_column_visitor<V: ColumnVisitor>(&self, visitor: &mut V) {
        T::accept_column_visitor(visitor)
    }
}

impl<'s, T> IntoRow<'s> for Option<T>
where
    T: IntoRow<'s>,
{
    type Serializer = OptionSerializer;
    fn get_serializer() -> Self::Serializer {
        OptionSerializer
    }
}

#[doc(hidden)]
pub struct OptionSerializer;

impl<'s, T> Serializer<'s, Option<T>> for OptionSerializer
where
    T: IntoRow<'s>,
{
    fn accept_cell_visitor<V: CellVisitor>(&self, value: &Option<T>, visitor: &mut V) {
        if let Some(item) = value.as_ref() {
            T::get_serializer().accept_cell_visitor(item, visitor);
        } else {
            let mut n = NullColumnVisitor { parent: visitor };
            T::get_serializer().accept_column_visitor(&mut n);
        }
    }

    fn accept_column_visitor<V: ColumnVisitor>(&self, visitor: &mut V) {
        T::get_serializer().accept_column_visitor(visitor);
    }
}

impl<'s, T> IntoRow<'s> for Arc<T>
where
    T: IntoRow<'s>,
{
    type Serializer = ArcSerializer;
    fn get_serializer() -> Self::Serializer {
        ArcSerializer
    }
}

#[doc(hidden)]
pub struct ArcSerializer;

impl<'r, T> Serializer<'r, Arc<T>> for ArcSerializer
where
    T: IntoRow<'r>,
{
    fn accept_cell_visitor<V: CellVisitor>(&self, value: &Arc<T>, visitor: &mut V) {
        T::get_serializer().accept_cell_visitor(&*value, visitor);
    }

    fn accept_column_visitor<V: ColumnVisitor>(&self, visitor: &mut V) {
        T::get_serializer().accept_column_visitor(visitor);
    }
}

/// Convert a structured value into a single representative [`CellValue`]
///
/// The conversion takes a `key`, so that different consumers can choose
/// different foreign keys to represent this type in their output.
pub trait AsForeignKey<'a, 'r>: Sized {
    type CellReducer: CellReducer<'a, 'r, Self>;
    fn get_cell_reducer(key: &'a str) -> Self::CellReducer;
}

impl<'a, 'r, T> AsForeignKey<'a, 'r> for T
where
    T: IntoRow<'r>,
{
    type CellReducer = SerializerCellReducer<'a, <T as IntoRow<'r>>::Serializer>;

    fn get_cell_reducer(key: &'a str) -> Self::CellReducer {
        SerializerCellReducer::new(T::get_serializer(), key)
    }
}

impl<'a, 'r, T> AsForeignKey<'a, 'r> for Vec<T>
where
    T: AsForeignKey<'a, 'r>,
{
    type CellReducer = VecCellReducer<<T as AsForeignKey<'a, 'r>>::CellReducer>;
    fn get_cell_reducer(key: &'a str) -> Self::CellReducer {
        VecCellReducer {
            nested: T::get_cell_reducer(key),
        }
    }
}

#[doc(hidden)]
pub trait CellReducer<'a, 'r, T> {
    /// Return the representation of the object if the column `name`
    /// of its own table is used to stand in for it.
    fn reduce_to_cell(&self, value: &T) -> CellValue;
}

#[doc(hidden)]
pub struct SerializerCellReducer<'a, S> {
    serializer: S,
    key: &'a str,
}

impl<'a, S> SerializerCellReducer<'a, S> {
    pub fn new(serializer: S, key: &'a str) -> Self {
        Self { serializer, key }
    }
}

impl<'a, 'r, S, T> CellReducer<'a, 'r, T> for SerializerCellReducer<'a, S>
where
    S: Serializer<'r, T>,
{
    fn reduce_to_cell(&self, value: &T) -> CellValue {
        let mut k = ForeignKeyVisitor {
            target: self.key,
            value: None,
        };
        self.serializer.accept_cell_visitor(value, &mut k);
        k.value.unwrap_or(CellValue::Null)
    }
}

#[doc(hidden)]
pub struct VecCellReducer<C> {
    nested: C,
}

impl<'a, 'r, T, C> CellReducer<'a, 'r, Vec<T>> for VecCellReducer<C>
where
    C: CellReducer<'a, 'r, T>,
{
    fn reduce_to_cell(&self, value: &Vec<T>) -> CellValue {
        CellValue::Array(
            value
                .iter()
                .map(|item| self.nested.reduce_to_cell(item))
                .collect(),
        )
    }
}

#[doc(hidden)]
pub struct ForeignKeyVisitor<'a> {
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

// Generic state support

/// Produce a [`Serializer`] to convert values of this type, which
/// requires a context value, into rows of output.
///
#[cfg_attr(
    feature = "persian-rug",
    doc = r##"
This can be derived via the
[`IntoRowWithPersianRug`](IntoRowWithPersianRug)
derive macro for the case of a [`persian-rug`](::persian_rug)
type.
"##
)]
pub trait IntoRowWithContext<'a, A: 'a>: Sized {
    type Serializer: Serializer<'a, Self>;
    /// `accessor` is some context value
    fn get_serializer(accessor: A) -> Self::Serializer;
}

/// Convert a structured value into a single representative
/// [`CellValue`] using a context value
///
/// The conversion takes a `key`, so that different consumers can choose
/// different foreign keys to represent this type in their output.
pub trait AsForeignKeyWithContext<'a, 'r, A: 'r>: Sized {
    type CellReducer: CellReducer<'a, 'r, Self>;
    fn get_cell_reducer(accessor: A, key: &'a str) -> Self::CellReducer;
}

impl<'a, 'r, A, T> AsForeignKeyWithContext<'a, 'r, A> for Vec<T>
where
    T: AsForeignKeyWithContext<'a, 'r, A>,
    A: 'r,
{
    type CellReducer = VecCellReducer<<T as AsForeignKeyWithContext<'a, 'r, A>>::CellReducer>;
    fn get_cell_reducer(accessor: A, key: &'a str) -> Self::CellReducer {
        VecCellReducer {
            nested: T::get_cell_reducer(accessor, key),
        }
    }
}

impl<'s, T, A> IntoRowWithContext<'s, A> for Option<T>
where
    T: IntoRowWithContext<'s, A>,
    A: 's + Clone,
{
    type Serializer = OptionSerializerWithContext<A>;
    fn get_serializer(access: A) -> Self::Serializer {
        OptionSerializerWithContext { access }
    }
}

impl<'a, 's, T, A> AsForeignKeyWithContext<'a, 's, A> for Option<T>
where
    T: IntoRowWithContext<'s, A>,
    A: 's + Clone,
{
    type CellReducer = SerializerCellReducer<'a, OptionSerializerWithContext<A>>;
    fn get_cell_reducer(access: A, key: &'a str) -> Self::CellReducer {
        SerializerCellReducer::new(OptionSerializerWithContext { access }, key)
    }
}

#[doc(hidden)]
pub struct OptionSerializerWithContext<A> {
    access: A,
}

impl<'s, T, A> Serializer<'s, Option<T>> for OptionSerializerWithContext<A>
where
    T: IntoRowWithContext<'s, A>,
    A: 's + Clone,
{
    fn accept_cell_visitor<V: CellVisitor>(&self, value: &Option<T>, visitor: &mut V) {
        if let Some(item) = value.as_ref() {
            T::get_serializer(self.access.clone()).accept_cell_visitor(item, visitor);
        } else {
            let mut n = NullColumnVisitor { parent: visitor };
            T::get_serializer(self.access.clone()).accept_column_visitor(&mut n);
        }
    }

    fn accept_column_visitor<V: ColumnVisitor>(&self, visitor: &mut V) {
        T::get_serializer(self.access.clone()).accept_column_visitor(visitor);
    }
}

pub use django_query_derive::IntoRow;

#[cfg(feature = "persian-rug")]
#[cfg_attr(docsrs, doc(cfg(feature = "persian-rug")))]
pub use crate::persian_rug::IntoRowWithPersianRug;
