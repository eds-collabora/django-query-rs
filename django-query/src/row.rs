use std::collections::BTreeMap;
use std::fmt::Display;
use std::sync::Arc;

use chrono::DateTime;
use serde_json::value::Value;
use serde_json::Number;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CellValue {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<CellValue>),
}

pub trait IntoCellValue {
    fn to_cell_value(&self) -> CellValue;
}

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
        CellValue::String(self.to_rfc3339())
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

pub trait CellVisitor {
    fn visit_value(&mut self, name: &str, v: CellValue);
}

pub trait ColumnVisitor {
    fn visit_column(&mut self, name: &str);
}

pub trait IntoRow {
    fn accept_cell_visitor<V: CellVisitor>(&self, visitor: &mut V);

    fn accept_column_visitor<V: ColumnVisitor>(visitor: &mut V);

    fn to_row(&self) -> BTreeMap<String, CellValue> {
        let mut r = RowVisitor {
            values: BTreeMap::new(),
        };
        self.accept_cell_visitor(&mut r);
        r.values
    }
    fn to_json(&self) -> Value {
        let mut j = JsonVisitor {
            value: serde_json::map::Map::new(),
        };
        self.accept_cell_visitor(&mut j);
        Value::Object(j.value)
    }
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
    T: IntoRow
{
    fn accept_cell_visitor<V: CellVisitor>(&self, visitor: &mut V) {
        T::accept_cell_visitor(&*self, visitor);
    }

    fn accept_column_visitor<V: ColumnVisitor>(visitor: &mut V) {
        T::accept_column_visitor(visitor);
    }
}
    

pub trait AsForeignKey {
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
        k.value.unwrap_or_else(|| CellValue::Null)
    }
}

impl<T> AsForeignKey for Vec<T>
where
    T: AsForeignKey,
{
    fn as_foreign_key(&self, name: &str) -> CellValue {
        let mut v = Vec::new();
        for item in self.into_iter() {
            v.push(item.as_foreign_key(name))
        }
        CellValue::Array(v)
    }
}

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
