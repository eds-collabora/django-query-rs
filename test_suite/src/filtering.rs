//#[cfg(test)]
use std::fmt::Debug;
use std::str::FromStr;

use django_query::filtering::*;
use django_query::*;

// struct MyRecord<T> {
//     string_field: String,
//     int_field: i32,
//     foo: T,
// }

// #[derive(Clone)]
// struct MyStringField;

// impl<T> Field<MyRecord<T>> for MyStringField
// where
//     T: Clone,
// {
//     type Value = String;
//     fn value<'a>(&self, data: &'a MyRecord<T>) -> &'a String {
//         &data.string_field
//     }
// }

// #[derive(Clone)]
// struct MyIntField;

// impl<T> Field<MyRecord<T>> for MyIntField
// where
//     T: Clone,
// {
//     type Value = i32;
//     fn value<'a>(&self, data: &'a MyRecord<T>) -> &'a i32 {
//         &data.int_field
//     }
// }

// #[derive(Clone)]
// struct MyTField;

// impl<T> Field<MyRecord<T>> for MyTField
// where
//     T: Clone,
// {
//     type Value = T;
//     fn value<'a>(&self, data: &'a MyRecord<T>) -> &'a T {
//         &data.foo
//     }
// }

// impl<T> Queryable for MyRecord<T>
// where
//     T: Clone + Equatable + FromStr + 'static,
//     <T as FromStr>::Err: std::error::Error + Debug + Send + Sync,
// {
//     fn create_metadata() -> QueryableRecord<Self> {
//         let mut qr = QueryableRecord::new();
//         let qf1 =
//             QueryableField::new(FilterClassImpl::new(MyStringField, operators::Eq));
//         let qf2 = QueryableField::new(FilterClassImpl::new(MyIntField, operators::Eq));
//         let qf3 = QueryableField::new(FilterClassImpl::new(MyTField, operators::Eq));
//         qr.add_field("string_field", qf1);
//         qr.add_field("int_field", qf2);
//         qr.add_field("foo", qf3);
//         qr
//     }
// }

// #[test]
// fn basic() {
//     let r = MyRecord {
//         string_field: "test".to_string(),
//         int_field: 0,
//         foo: "hello",
//     };
//     let sfield = MyStringField;
//     let opcls = operators::Eq;
//     let op = OperatorClass::<String>::instantiate(&opcls, "test").unwrap();
//     let filter = FilterImpl::new(sfield, op);
//     assert!(filter.filter_one(&r));
// }

// #[test]
// fn generic() {
//     let r = MyRecord {
//         string_field: "test".to_string(),
//         int_field: 0,
//         foo: 5,
//     };
//     let sfield = MyTField;
//     let opcls = operators::Eq;
//     let op = OperatorClass::<i32>::instantiate(&opcls, "5").unwrap();
//     let filter = FilterImpl::new(sfield, op);
//     assert!(filter.filter_one(&r));
// }

// #[test]
// fn records() {
//     let r = MyRecord {
//         string_field: "test".to_string(),
//         int_field: 0,
//         foo: 0,
//     };
//     let mut qr = QueryableRecord::new();
//     let qf1 = QueryableField::new(FilterClassImpl::new(MyStringField, operators::Eq));
//     qr.add_field("string_field", qf1);

//     let filter = qr.create_filter("string_field", None, "test").unwrap();
//     assert!(filter.filter_one(&r));
// }

// #[test]
// fn using_trait() {
//     let r = MyRecord {
//         string_field: "test3".to_string(),
//         int_field: 4,
//         foo: "hi".to_string(),
//     };
//     let qr = MyRecord::create_metadata();

//     let filter = qr.create_filter("int_field", None, "4").unwrap();
//     assert!(filter.filter_one(&r));
// }

/* What's missing here are:
- attributes to rename fields
- attributes to select operators, or groups of operators per field
 */
#[derive(Queryable)]
struct MyRecord2<T>
where
    T: Operable + 'static,
    <T as Operable>::Base: core::cmp::Eq + FromStr,
    <<T as Operable>::Base as FromStr>::Err: Debug + std::error::Error + Sync + Send + 'static,
{
    #[django(rename = "MYSTRING")]
    string_field: String,
    int_field: i32,
    #[django(rename="bar", op(in=operators::In))]
    foo: T,
}

#[test]
fn using_macro() {
    let r = MyRecord2 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: 11,
    };
    let qr = QueryableRecord::<MyRecord2<_>>::new();

    let filter = qr.create_filter("int_field", None, "4").unwrap();
    assert!(filter.filter_one(&r));

    let filter = qr.create_filter("bar", None, "11").unwrap();
    assert!(filter.filter_one(&r));
}

#[test]
fn using_filter() {
    let r = MyRecord2 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: 11,
    };
    let r2 = MyRecord2 {
        string_field: "test3".to_string(),
        int_field: -1,
        foo: 12,
    };
    let qr = QueryableRecord::<MyRecord2<_>>::new();
    let filter = qr.create_filter_from_query_pair("int_field", "4").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));

    let filter = qr.create_filter_from_query("bar=11").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
}

#[test]
fn using_in() {
    let r = MyRecord2 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: 11,
    };
    let r2 = MyRecord2 {
        string_field: "test3".to_string(),
        int_field: -1,
        foo: 12,
    };
    let r3 = MyRecord2 {
        string_field: "test3".to_string(),
        int_field: -1,
        foo: 1,
    };
    let qr = QueryableRecord::<MyRecord2<_>>::new();
    let filter = qr.create_filter_from_query("bar__in=1,2,11").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));
}

#[derive(Queryable)]
struct MyRecord3<T>
where
    T: Operable + 'static,
    <T as Operable>::Base: core::cmp::Eq + FromStr,
    <<T as Operable>::Base as FromStr>::Err: Debug + std::error::Error + Sync + Send + 'static,
{
    #[django(rename = "MYSTRING")]
    string_field: String,
    #[django(default_op = in)]
    int_field: i32,
    #[django(rename = "bar", op(in))]
    foo: T,
}

#[test]
fn using_in2() {
    let r = MyRecord3 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: 11,
    };
    let r2 = MyRecord3 {
        string_field: "test3".to_string(),
        int_field: -1,
        foo: 12,
    };
    let r3 = MyRecord3 {
        string_field: "test3".to_string(),
        int_field: -1,
        foo: 1,
    };
    let qr = QueryableRecord::<MyRecord3<_>>::new();
    let filter = qr.create_filter_from_query("bar__in=1,2,11").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));
}

#[test]
fn using_default_op1() {
    let r = MyRecord3 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: 11,
    };
    let r2 = MyRecord3 {
        string_field: "test3".to_string(),
        int_field: -1,
        foo: 12,
    };
    let r3 = MyRecord3 {
        string_field: "test3".to_string(),
        int_field: 1,
        foo: 1,
    };
    let qr = QueryableRecord::<MyRecord3<_>>::new();
    let filter = qr.create_filter_from_query("int_field=1,4").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));
}

#[derive(Queryable)]
struct MyRecord4<T>
where
    T: Operable + 'static,
    <T as Operable>::Base: core::cmp::Eq + FromStr,
    <<T as Operable>::Base as FromStr>::Err: Debug + std::error::Error + Sync + Send + 'static,
{
    #[django(
        rename = "MYSTRING",
        op(contains),
        op(icontains),
        op(startswith),
        op(endswith)
    )]
    string_field: String,
    #[django(default_fun = operators::In, op(eq,lt,lte,gt,gte=operators::GreaterEq))]
    int_field: i32,
    #[django(rename = "bar", op(in))]
    foo: T,
}

#[test]
fn using_default_op2() {
    let r = MyRecord4 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: 11,
    };
    let r2 = MyRecord4 {
        string_field: "test3".to_string(),
        int_field: -1,
        foo: 12,
    };
    let r3 = MyRecord4 {
        string_field: "test3".to_string(),
        int_field: 1,
        foo: 1,
    };
    let qr = QueryableRecord::<MyRecord4<_>>::new();
    let filter = qr.create_filter_from_query("int_field=1,4").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));
}

#[test]
fn using_more_operators() {
    let r = MyRecord4 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: 11,
    };
    let r2 = MyRecord4 {
        string_field: "test2".to_string(),
        int_field: -1,
        foo: 12,
    };
    let r3 = MyRecord4 {
        string_field: "TEST3".to_string(),
        int_field: 1,
        foo: 1,
    };
    let qr = QueryableRecord::<MyRecord4<_>>::new();
    let filter = qr.create_filter_from_query("int_field__eq=1").unwrap();
    assert!(!filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));

    let filter = qr.create_filter_from_query("int_field__lt=1").unwrap();
    assert!(!filter.filter_one(&r));
    assert!(filter.filter_one(&r2));
    assert!(!filter.filter_one(&r3));

    let filter = qr.create_filter_from_query("int_field__lte=1").unwrap();
    assert!(!filter.filter_one(&r));
    assert!(filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));

    let filter = qr.create_filter_from_query("int_field__gt=1").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(!filter.filter_one(&r3));

    let filter = qr.create_filter_from_query("int_field__gte=1").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));

    let filter = qr
        .create_filter_from_query("MYSTRING__contains=est")
        .unwrap();
    assert!(filter.filter_one(&r));
    assert!(filter.filter_one(&r2));
    assert!(!filter.filter_one(&r3));

    let filter = qr
        .create_filter_from_query("MYSTRING__icontains=EST")
        .unwrap();
    assert!(filter.filter_one(&r));
    assert!(filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));

    let filter = qr
        .create_filter_from_query("MYSTRING__startswith=tes")
        .unwrap();
    assert!(filter.filter_one(&r));
    assert!(filter.filter_one(&r2));
    assert!(!filter.filter_one(&r3));

    let filter = qr.create_filter_from_query("MYSTRING__endswith=3").unwrap();
    assert!(filter.filter_one(&r));
    assert!(!filter.filter_one(&r2));
    assert!(filter.filter_one(&r3));
}

#[derive(Queryable)]
struct MyNestedRecord {
    string_field: String,
    int_field: i32,
}

#[derive(Queryable)]
struct MyRecord5 {
    string_field: String,
    int_field: i32,
    #[django(traverse)]
    foo: MyNestedRecord,
}

#[test]
fn basic_nesting() {
    let r = MyRecord5 {
        string_field: "test3".to_string(),
        int_field: 4,
        foo: MyNestedRecord {
            string_field: "test4".to_string(),
            int_field: 11,
        },
    };
    let qr = QueryableRecord::<MyRecord5>::new();
    let filter = qr.create_filter_from_query("foo__int_field=11").unwrap();
    assert!(filter.filter_one(&r));
    let filter = qr.create_filter_from_query("foo__int_field=4").unwrap();
    assert!(!filter.filter_one(&r));
    let filter = qr.create_filter_from_query("int_field=4").unwrap();
    assert!(filter.filter_one(&r));
    let filter = qr.create_filter_from_query("int_field=5").unwrap();
    assert!(!filter.filter_one(&r));
}

#[derive(Queryable)]
struct MyRecord6 {
    string_field: String,
    #[django(traverse)]
    foo: Vec<MyNestedRecord>,
}

#[test]
fn vector_nesting() {
    let r = MyRecord6 {
        string_field: "hello".to_string(),
        foo: vec![
            MyNestedRecord {
                string_field: "one".to_string(),
                int_field: 1,
            },
            MyNestedRecord {
                string_field: "two".to_string(),
                int_field: 2,
            },
            MyNestedRecord {
                string_field: "three".to_string(),
                int_field: 3,
            },
        ],
    };
    print_queryable::<MyRecord6>();

    let qr = QueryableRecord::<MyRecord6>::new();
    let filter = qr.create_filter_from_query("foo__int_field=1").unwrap();
    assert!(filter.filter_one(&r));
    let filter = qr.create_filter_from_query("foo__int_field=2").unwrap();
    assert!(filter.filter_one(&r));
    let filter = qr.create_filter_from_query("foo__int_field=3").unwrap();
    assert!(filter.filter_one(&r));
    let filter = qr.create_filter_from_query("foo__int_field=4").unwrap();
    assert!(!filter.filter_one(&r));
}
