#![allow(unused_imports)]

use django_query::*;

use django_query::row::*;

use serde_json::value::Value;
use serde_json::Number;

use std::collections::BTreeMap;
use std::fmt::Debug;

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
fn equal_maps<X: Eq + Ord + Debug, Y: Eq + Debug>(a: &BTreeMap<X, Y>, b: &BTreeMap<X, Y>) -> bool {
    if !(subset(a, b) && subset(b, a)) {
        eprintln!("LHS: {:?}", a);
        eprintln!("RHS: {:?}", b);
        false
    } else {
        true
    }
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
    #[django(foreign_key="int_field")]
    nest: Record,
    string_field: String,
    int_field: i32
}

#[derive(IntoRow)]
struct Record3 {
    #[django(foreign_key="string_field")]
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

#[test]
fn test_columns() {
    assert_eq!(Record::columns(), vec!["string_field".to_string(), "int_field".to_string()]);
    assert_eq!(Record2::columns(), vec!["nest".to_string(), "string_field".to_string(), "int_field".to_string()]);
    assert_eq!(Record3::columns(), vec!["nest".to_string(), "string_field".to_string(), "int_field".to_string()]);
    assert_eq!(Record4::columns(), vec!["string_field".to_string(), "int_field".to_string()]);
}

#[derive(IntoRow)]
struct Record6 {
    #[django(rename="womble")]
    string_field: String,
    int_field: i32
}

#[derive(IntoRow)]
struct Record5 {
    #[django(rename="NESTED_STRING",foreign_key="womble")]
    nest: Record6,
    #[django(rename="OUTER_STRING")]
    string_field: String,
    int_field: i32
}

#[test]
fn test_rename() {
    assert_eq!(Record5::columns(), vec!["NESTED_STRING".to_string(), "OUTER_STRING".to_string(), "int_field".to_string()]);

    let r = Record5 { nest: Record6 { string_field: "nesting".to_string(), int_field: 15 }, string_field: "hello".to_string(), int_field: 1 };

    let v = r.to_row();

    let compare = BTreeMap::from([
        ("OUTER_STRING".to_string(), CellValue::String("hello".to_string())),
        ("int_field".to_string(), CellValue::Number(Number::from(1i32))),
        ("NESTED_STRING".to_string(), CellValue::String("nesting".to_string())),
    ]);
    
    assert!(equal_maps(&v, &compare));
}

#[derive(IntoRow)]
struct Record7 {
    #[django(rename="NESTED_STRING", foreign_key="womble")]
    nest: Vec<Record6>,
    #[django(rename="OUTER_STRING")]
    string_field: String,
    int_field: i32
}

#[test]
fn test_array() {
    assert_eq!(Record7::columns(), vec!["NESTED_STRING".to_string(), "OUTER_STRING".to_string(), "int_field".to_string()]);

    let r = Record7 {
        nest: vec![
            Record6 { string_field: "nesting".to_string(), int_field: 15 },
            Record6 { string_field: "nosting".to_string(), int_field: 1 },
            Record6 { string_field: "nisting".to_string(), int_field: 12 },
            Record6 { string_field: "nasting".to_string(), int_field: 11 },
            Record6 { string_field: "nusting".to_string(), int_field: 52 }
        ] ,
        string_field: "hello".to_string(),
        int_field: 1
    };

    let v = r.to_row();

    let compare = BTreeMap::from([
        ("OUTER_STRING".to_string(), CellValue::String("hello".to_string())),
        ("int_field".to_string(), CellValue::Number(Number::from(1i32))),
        ("NESTED_STRING".to_string(),
         CellValue::Array(
             vec![
                 CellValue::String("nesting".to_string()),
                 CellValue::String("nosting".to_string()),
                 CellValue::String("nisting".to_string()),
                 CellValue::String("nasting".to_string()),
                 CellValue::String("nusting".to_string()),
             ])
        )
    ]);

    assert!(equal_maps(&v, &compare));
}
    
