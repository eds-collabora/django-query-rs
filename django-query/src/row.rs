/*
Representation of output data. Foreign key handling is the main reason
you want this.
 */

use std::collections::BTreeMap;
use std::fmt::Display;

use serde_json::Number;
use serde_json::value::Value;

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum CellValue {
    Null,
    Bool(bool),
    Number(Number),
    String(String)
}

pub trait IntoCellValue {
    fn to_cell_value(&self) -> CellValue;
}

pub trait StringCellValue {}

impl StringCellValue for String {}

impl<T> IntoCellValue for T
where
    T: StringCellValue + Display
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
        serde_json::Number::from_f64((*self).into()).map(CellValue::Number).unwrap_or_else(|| CellValue::Null)
    }
}

impl IntoCellValue for f64 {
    fn to_cell_value(&self) -> CellValue {
        serde_json::Number::from_f64(*self).map(CellValue::Number).unwrap_or_else(|| CellValue::Null)
    }
}

pub trait Visitor {
    fn visit_value(&mut self, name: &str, v: CellValue);
}

pub trait IntoRow {
    fn accept_visitor<V: Visitor>(&self, visitor: &mut V);

    fn to_row(&self) -> BTreeMap<String, CellValue> {
        let mut r = RowVisitor { values: BTreeMap::new() };
        self.accept_visitor(&mut r);
        r.values
    }
    fn to_json(&self) -> Value {
        let mut j = JsonVisitor { value: serde_json::map::Map::new() };
        self.accept_visitor(&mut j);
        Value::Object(j.value)
    }
}

pub struct KeyVisitor<'a> {
    pub target: &'a str,
    pub value: Option<CellValue>
}

impl<'a> Visitor for KeyVisitor<'a> {
    fn visit_value(&mut self, name: &str, v: CellValue) {
        if name == self.target {
            self.value = Some(v);
        }
    }
}

struct RowVisitor {
    pub values: BTreeMap<String, CellValue>
}

impl Visitor for RowVisitor {
    fn visit_value(&mut self, name: &str, v: CellValue) {
        self.values.insert(name.to_string(), v);
    }
}

struct JsonVisitor {
    pub value: serde_json::map::Map<String, Value>
}

impl Visitor for JsonVisitor {
    fn visit_value(&mut self, name: &str, v: CellValue) {
        let v = match v {
            CellValue::Null => Value::Null,
            CellValue::Bool(b) => Value::Bool(b),
            CellValue::Number(n) => Value::Number(n),
            CellValue::String(s) => Value::String(s)
        };
        self.value.insert(name.to_string(), v);
    }
}
