#![allow(unused_imports)]

use django_query::*;

use django_query::row::*;

use serde_json::value::Value;
use serde_json::Number;

use std::collections::BTreeMap;

#[derive(IntoRow)]
struct Record {
    string_field: String,
    int_field: i32
}

#[cfg(test)]
fn subset<X: Eq + Ord, Y: Eq>(a: &BTreeMap<X, Y>, b: &BTreeMap<X, Y>) -> bool {
    for (k, v) in a.iter() {
        if let Some(w) = b.get(k) {
            if w != v {
                return false;
            }
        } else {
            return false;
        }
    }
    true
}
    
#[cfg(test)]
fn equal_maps<X: Eq + Ord, Y: Eq>(a: &BTreeMap<X, Y>, b: &BTreeMap<X, Y>) -> bool {
    subset(a, b) && subset(b, a)
}
    
#[test]
fn test_basic() {
    let r = Record { string_field: "hello".to_string(), int_field: 1 };

    let v = r.to_row();

    let compare = BTreeMap::from([
        ("string_field".to_string(), CellValue::String("hello".to_string())),
        ("int_field".to_string(), CellValue::Number(Number::from(1i32)))
    ]);

    assert!(equal_maps(&v, &compare));
}

#[derive(IntoRow)]
struct Record2 {
    #[django(foreign_key=int_field)]
    nest: Record,
    string_field: String,
    int_field: i32
}

#[derive(IntoRow)]
struct Record3 {
    #[django(foreign_key=string_field)]
    nest: Record,
    string_field: String,
    int_field: i32
}

#[test]
fn test_nesting() {
    let r = Record2 { nest: Record { string_field: "nesting".to_string(), int_field: 15 }, string_field: "hello".to_string(), int_field: 1 };

    let v = r.to_row();

    let compare = BTreeMap::from([
        ("string_field".to_string(), CellValue::String("hello".to_string())),
        ("int_field".to_string(), CellValue::Number(Number::from(1i32))),
        ("nest".to_string(), CellValue::Number(Number::from(15i32))),
    ]);

    assert!(equal_maps(&v, &compare));
    
    let r = Record3 { nest: Record { string_field: "nesting".to_string(), int_field: 15 }, string_field: "hello".to_string(), int_field: 1 };

    let v = r.to_row();

    let compare = BTreeMap::from([
        ("string_field".to_string(), CellValue::String("hello".to_string())),
        ("int_field".to_string(), CellValue::Number(Number::from(1i32))),
        ("nest".to_string(), CellValue::String("nesting".to_string())),
    ]);
    
    assert!(equal_maps(&v, &compare));
}

#[derive(IntoRow)]
struct Record4 {
    #[allow(dead_code)]
    #[django(exclude)]
    nest: Record,
    string_field: String,
    int_field: i32
}

#[test]
fn test_exclude() {
    let r = Record4 { nest: Record { string_field: "nesting".to_string(), int_field: 15 }, string_field: "hello".to_string(), int_field: 1 };

    let v = r.to_row();

    let compare = BTreeMap::from([
        ("string_field".to_string(), CellValue::String("hello".to_string())),
        ("int_field".to_string(), CellValue::Number(Number::from(1i32))),
    ]);
    
    assert!(equal_maps(&v, &compare));
}

    
