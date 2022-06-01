use django_query::sorting::{self, Sortable};

struct MyRecord {
    string_field: String,
    int_field: i32,
}

#[derive(Clone)]
struct StringField;

impl sorting::Accessor<MyRecord> for StringField {
    type Value = String;
    fn value<'a>(&self, data: &'a MyRecord) -> &'a String {
        &data.string_field
    }
}
impl sorting::ReferenceField for StringField {}

#[derive(Clone)]
struct IntField;

impl sorting::Accessor<MyRecord> for IntField {
    type Value = i32;
    fn value<'a>(&self, data: &'a MyRecord) -> &'a i32 {
        &data.int_field
    }
}
impl sorting::ReferenceField for IntField {}

struct MyRecordMeta;

impl<'s> sorting::Sortable<'s> for MyRecord {
    type Meta = MyRecordMeta;
    fn get_meta() -> Self::Meta {
        MyRecordMeta
    }
}

impl<'s> sorting::Meta<'s, MyRecord> for MyRecordMeta {
    fn accept_visitor<V: sorting::SortVisitor<'s, Target = MyRecord>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        visitor.visit_sort("string_field", &StringField, &sorting::CompareClass);
        visitor.visit_sort("int_field", &IntField, &sorting::CompareClass);
    }
}

#[test]
fn test_basic() {
    let mut v = vec![
        MyRecord {
            string_field: "a".to_string(),
            int_field: 9,
        },
        MyRecord {
            string_field: "b".to_string(),
            int_field: 1,
        },
        MyRecord {
            string_field: "c".to_string(),
            int_field: 4,
        },
    ];

    let sr = sorting::OrderingSet::<MyRecord>::new();

    let sort = sr.create_sort("int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 1);
    assert_eq!(v[1].int_field, 4);
    assert_eq!(v[2].int_field, 9);

    let sort = sr.create_sort("-int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 9);
    assert_eq!(v[1].int_field, 4);
    assert_eq!(v[2].int_field, 1);

    let sort = sr.create_sort("string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 9);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 4);

    let sort = sr.create_sort("-string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 9);
}

struct MyRecord2 {
    foo: MyRecord,
    int_field: i32,
}

#[derive(Clone)]
struct FooField2;

impl sorting::Accessor<MyRecord2> for FooField2 {
    type Value = MyRecord;
    fn value<'a>(&self, data: &'a MyRecord2) -> &'a MyRecord {
        &data.foo
    }
}
impl sorting::ReferenceField for FooField2 {}

#[derive(Clone)]
struct IntField2;

impl sorting::Accessor<MyRecord2> for IntField2 {
    type Value = i32;
    fn value<'a>(&self, data: &'a MyRecord2) -> &'a i32 {
        &data.int_field
    }
}
impl sorting::ReferenceField for IntField2 {}

struct MyRecord2Meta;

impl<'s> sorting::Sortable<'s> for MyRecord2 {
    type Meta = MyRecord2Meta;
    fn get_meta() -> Self::Meta {
        MyRecord2Meta
    }
}

impl<'s> sorting::Meta<'s, MyRecord2> for MyRecord2Meta {
    fn accept_visitor<V: sorting::SortVisitor<'s, Target = MyRecord2>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        visitor.visit_key_sort("foo", &FooField2, "string_field", MyRecordMeta);
        visitor.visit_sort("int_field", &IntField2, &sorting::CompareClass);
    }
}

#[test]
fn test_nesting() {
    let mut v = vec![
        MyRecord2 {
            foo: MyRecord {
                string_field: "a".to_string(),
                int_field: 9,
            },
            int_field: 5,
        },
        MyRecord2 {
            foo: MyRecord {
                string_field: "b".to_string(),
                int_field: 1,
            },
            int_field: 1,
        },
        MyRecord2 {
            foo: MyRecord {
                string_field: "c".to_string(),
                int_field: 4,
            },
            int_field: 3,
        },
    ];

    let sr = sorting::OrderingSet::<MyRecord2>::new();

    let sort = sr.create_sort("int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 1);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 5);

    let sort = sr.create_sort("-int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 5);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 1);

    let sort = sr.create_sort("foo").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 5);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 3);

    let sort = sr.create_sort("-foo").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 3);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 5);
}

struct MyRecord3 {
    bar: MyRecord2,
    foo: MyRecord,
    int_field: i32,
    string_field: String,
}

#[derive(Clone)]
struct BarField3;

impl sorting::Accessor<MyRecord3> for BarField3 {
    type Value = MyRecord2;
    fn value<'a>(&self, data: &'a MyRecord3) -> &'a MyRecord2 {
        &data.bar
    }
}
impl sorting::ReferenceField for BarField3 {}

#[derive(Clone)]
struct FooField3;

impl sorting::Accessor<MyRecord3> for FooField3 {
    type Value = MyRecord;
    fn value<'a>(&self, data: &'a MyRecord3) -> &'a MyRecord {
        &data.foo
    }
}
impl sorting::ReferenceField for FooField3 {}

#[derive(Clone)]
struct IntField3;

impl sorting::Accessor<MyRecord3> for IntField3 {
    type Value = i32;
    fn value<'a>(&self, data: &'a MyRecord3) -> &'a i32 {
        &data.int_field
    }
}
impl sorting::ReferenceField for IntField3 {}

#[derive(Clone)]
struct StringField3;

impl sorting::Accessor<MyRecord3> for StringField3 {
    type Value = String;
    fn value<'a>(&self, data: &'a MyRecord3) -> &'a String {
        &data.string_field
    }
}
impl sorting::ReferenceField for StringField3 {}

struct MyRecord3Meta;

impl<'s> sorting::Sortable<'s> for MyRecord3 {
    type Meta = MyRecord3Meta;

    fn get_meta() -> Self::Meta {
        MyRecord3Meta
    }
}

impl<'s> sorting::Meta<'s, MyRecord3> for MyRecord3Meta {
    fn accept_visitor<V: sorting::SortVisitor<'s, Target = MyRecord3>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        visitor.visit_key_sort("foo", &FooField3, "int_field", MyRecordMeta);
        visitor.visit_key_sort("bar", &BarField3, "foo", MyRecord2Meta);
        visitor.visit_sort("int_field", &IntField3, &sorting::CompareClass);
        visitor.visit_sort("string_field", &StringField3, &sorting::CompareClass);
    }
}

#[test]
fn test_deeper_nesting() {
    let mut v = vec![
        MyRecord3 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "a".to_string(),
                    int_field: 1,
                },
                int_field: 1,
            },
            foo: MyRecord {
                string_field: "a".to_string(),
                int_field: 3,
            },
            int_field: 2,
            string_field: "d".to_string(),
        },
        MyRecord3 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "b".to_string(),
                    int_field: 2,
                },
                int_field: 3,
            },
            foo: MyRecord {
                string_field: "b".to_string(),
                int_field: 2,
            },
            int_field: 1,
            string_field: "c".to_string(),
        },
        MyRecord3 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "c".to_string(),
                    int_field: 4,
                },
                int_field: 2,
            },
            foo: MyRecord {
                string_field: "d".to_string(),
                int_field: 1,
            },
            int_field: 3,
            string_field: "a".to_string(),
        },
        MyRecord3 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "d".to_string(),
                    int_field: 3,
                },
                int_field: 4,
            },
            foo: MyRecord {
                string_field: "c".to_string(),
                int_field: 4,
            },
            int_field: 4,
            string_field: "b".to_string(),
        },
    ];

    let sr = sorting::OrderingSet::<MyRecord3>::new();

    let sort = sr.create_sort("int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 1);
    assert_eq!(v[1].int_field, 2);
    assert_eq!(v[2].int_field, 3);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 2);
    assert_eq!(v[3].int_field, 1);

    let sort = sr.create_sort("string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 3);
    assert_eq!(v[1].int_field, 4);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 2);

    let sort = sr.create_sort("-string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 2);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 4);
    assert_eq!(v[3].int_field, 3);

    let sort = sr.create_sort("foo").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 3);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 2);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-foo").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 2);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 3);

    let sort = sr.create_sort("bar").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 2);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 3);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-bar").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 2);
}

#[derive(Sortable)]
struct MyRecord4 {
    #[django(sort("foo"))]
    bar: MyRecord2,
    #[django(sort("int_field"))]
    foo: MyRecord,
    #[django(sort)]
    int_field: i32,
    #[django(sort)]
    string_field: String,
}

#[test]
fn test_macro() {
    let mut v = vec![
        MyRecord4 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "a".to_string(),
                    int_field: 1,
                },
                int_field: 1,
            },
            foo: MyRecord {
                string_field: "a".to_string(),
                int_field: 3,
            },
            int_field: 2,
            string_field: "d".to_string(),
        },
        MyRecord4 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "b".to_string(),
                    int_field: 2,
                },
                int_field: 3,
            },
            foo: MyRecord {
                string_field: "b".to_string(),
                int_field: 2,
            },
            int_field: 1,
            string_field: "c".to_string(),
        },
        MyRecord4 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "c".to_string(),
                    int_field: 4,
                },
                int_field: 2,
            },
            foo: MyRecord {
                string_field: "d".to_string(),
                int_field: 1,
            },
            int_field: 3,
            string_field: "a".to_string(),
        },
        MyRecord4 {
            bar: MyRecord2 {
                foo: MyRecord {
                    string_field: "d".to_string(),
                    int_field: 3,
                },
                int_field: 4,
            },
            foo: MyRecord {
                string_field: "c".to_string(),
                int_field: 4,
            },
            int_field: 4,
            string_field: "b".to_string(),
        },
    ];

    let sr = sorting::OrderingSet::<MyRecord4>::new();

    let sort = sr.create_sort("int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 1);
    assert_eq!(v[1].int_field, 2);
    assert_eq!(v[2].int_field, 3);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 2);
    assert_eq!(v[3].int_field, 1);

    let sort = sr.create_sort("string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 3);
    assert_eq!(v[1].int_field, 4);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 2);

    let sort = sr.create_sort("-string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 2);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 4);
    assert_eq!(v[3].int_field, 3);

    let sort = sr.create_sort("foo").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 3);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 2);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-foo").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 2);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 3);

    let sort = sr.create_sort("bar").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 2);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 3);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-bar").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 2);
}

#[derive(Sortable)]
struct MyRecord6 {
    #[django(rename = "foot", sort("string_field"))]
    foo: MyRecord,
    #[django(rename = "i32", sort)]
    int_field: i32,
}

#[derive(Sortable)]
struct MyRecord5 {
    #[django(rename = "barbar", sort("foot"))]
    bar: MyRecord6,
    #[django(rename = "womble", sort("int_field"))]
    foo: MyRecord,
    #[django(rename = "int", sort)]
    int_field: i32,
    #[django(rename = "str", sort)]
    string_field: String,
}

#[test]
fn test_rename() {
    let mut v = vec![
        MyRecord5 {
            bar: MyRecord6 {
                foo: MyRecord {
                    string_field: "a".to_string(),
                    int_field: 1,
                },
                int_field: 1,
            },
            foo: MyRecord {
                string_field: "a".to_string(),
                int_field: 3,
            },
            int_field: 2,
            string_field: "d".to_string(),
        },
        MyRecord5 {
            bar: MyRecord6 {
                foo: MyRecord {
                    string_field: "b".to_string(),
                    int_field: 2,
                },
                int_field: 3,
            },
            foo: MyRecord {
                string_field: "b".to_string(),
                int_field: 2,
            },
            int_field: 1,
            string_field: "c".to_string(),
        },
        MyRecord5 {
            bar: MyRecord6 {
                foo: MyRecord {
                    string_field: "c".to_string(),
                    int_field: 4,
                },
                int_field: 2,
            },
            foo: MyRecord {
                string_field: "d".to_string(),
                int_field: 1,
            },
            int_field: 3,
            string_field: "a".to_string(),
        },
        MyRecord5 {
            bar: MyRecord6 {
                foo: MyRecord {
                    string_field: "d".to_string(),
                    int_field: 3,
                },
                int_field: 4,
            },
            foo: MyRecord {
                string_field: "c".to_string(),
                int_field: 4,
            },
            int_field: 4,
            string_field: "b".to_string(),
        },
    ];

    let sr = sorting::OrderingSet::<MyRecord5>::new();

    let sort = sr.create_sort("int").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 1);
    assert_eq!(v[1].int_field, 2);
    assert_eq!(v[2].int_field, 3);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-int").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 2);
    assert_eq!(v[3].int_field, 1);

    let sort = sr.create_sort("str").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 3);
    assert_eq!(v[1].int_field, 4);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 2);

    let sort = sr.create_sort("-str").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 2);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 4);
    assert_eq!(v[3].int_field, 3);

    let sort = sr.create_sort("womble").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 3);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 2);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-womble").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 2);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 3);

    let sort = sr.create_sort("barbar").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 2);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 3);
    assert_eq!(v[3].int_field, 4);

    let sort = sr.create_sort("-barbar").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 3);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[3].int_field, 2);
}

#[test]
fn test_double() {
    let mut v = vec![
        MyRecord {
            string_field: "b".to_string(),
            int_field: 1,
        },
        MyRecord {
            string_field: "a".to_string(),
            int_field: 1,
        },
        MyRecord {
            string_field: "b".to_string(),
            int_field: 4,
        },
    ];

    let sr = sorting::OrderingSet::<MyRecord>::new();

    let sort = sr.create_sort("int_field,string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 1);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 4);
    assert_eq!(v[0].string_field, "a".to_string());
    assert_eq!(v[1].string_field, "b".to_string());
    assert_eq!(v[2].string_field, "b".to_string());

    let sort = sr.create_sort("-int_field,-string_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 1);
    assert_eq!(v[0].string_field, "b".to_string());
    assert_eq!(v[1].string_field, "b".to_string());
    assert_eq!(v[2].string_field, "a".to_string());

    let sort = sr.create_sort("string_field,int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].string_field, "a".to_string());
    assert_eq!(v[1].string_field, "b".to_string());
    assert_eq!(v[2].string_field, "b".to_string());
    assert_eq!(v[0].int_field, 1);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 4);

    let sort = sr.create_sort("-string_field,-int_field").unwrap();
    sort.sort_vec(&mut v);
    assert_eq!(v[0].string_field, "b".to_string());
    assert_eq!(v[1].string_field, "b".to_string());
    assert_eq!(v[2].string_field, "a".to_string());
    assert_eq!(v[0].int_field, 4);
    assert_eq!(v[1].int_field, 1);
    assert_eq!(v[2].int_field, 1);
}
