use std::collections::BTreeMap;
use std::fmt::Debug;
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum FilterError {
    #[error("malformed query has no '='")]
    MissingEquals,
    #[error("no such operator '{0}'")]
    NoOperator(String),
    #[error("no such field '{0}'")]
    NoField(String),
    #[error(transparent)]
    Instantiation(#[from] anyhow::Error),
}

pub trait Field<R> {
    type Value;
    fn value<'a>(&'_ self, data: &'a R) -> &'a Self::Value;
}

pub trait Operator<T> {
    fn apply(&self, value: &T) -> bool;
}

pub trait OperatorClass<T> {
    type Instance: Operator<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError>;
}

pub trait Filter<R> {
    fn filter_one(&self, data: &R) -> bool;
    fn filter_vec(&self, data: &mut Vec<R>) {
        data.retain(|r| self.filter_one(r))
    }
}

pub trait FilterClass<R: ?Sized> {
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R>>, FilterError>;
}

pub trait Queryable {
    fn create_metadata() -> QueryableRecord<Self>;

    fn create_filter_from_query_pair(
        lhs: &str,
        rhs: &str,
    ) -> Result<Box<dyn Filter<Self>>, FilterError> {
        let query_parts = lhs.splitn(2, "__").collect::<Vec<_>>();
        if query_parts.len() < 2 {
            Self::create_metadata().create_filter(lhs, None, rhs)
        } else {
            Self::create_metadata().create_filter(query_parts[0], Some(query_parts[1]), rhs)
        }
    }

    fn create_filter_from_query(expr: &str) -> Result<Box<dyn Filter<Self>>, FilterError> {
        let parts = expr.splitn(2, '=').collect::<Vec<_>>();
        if parts.len() != 2 {
            Err(FilterError::MissingEquals)
        } else {
            Self::create_filter_from_query_pair(parts[0], parts[1])
        }
    }
}

pub(crate) struct FilterImpl<F, O> {
    field: F,
    operator: O,
}

impl<F, O> FilterImpl<F, O> {
    pub fn new(field: F, operator: O) -> Self {
        Self { field, operator }
    }
}

impl<F, O, T, R> Filter<R> for FilterImpl<F, O>
where
    F: Field<R, Value = T>,
    O: Operator<T>,
{
    fn filter_one(&self, data: &R) -> bool {
        self.operator.apply(self.field.value(data))
    }
}

pub(crate) struct FilterClassImpl<F, O> {
    field: F,
    opclass: O,
}

impl<F, O> FilterClassImpl<F, O> {
    pub fn new(field: F, opclass: O) -> Self {
        Self { field, opclass }
    }
}

impl<F, O, R, T> FilterClass<R> for FilterClassImpl<F, O>
where
    F: Field<R, Value = T> + Clone + 'static,
    O: OperatorClass<T>,
    <O as OperatorClass<T>>::Instance: 'static,
{
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R>>, FilterError> {
        Ok(Box::new(FilterImpl::new(
            self.field.clone(),
            self.opclass.instantiate(rhs)?,
        )))
    }
}

pub struct QueryableField<R: ?Sized> {
    default: Box<dyn FilterClass<R>>,
    operators: BTreeMap<String, Box<dyn FilterClass<R>>>,
}

impl<R: ?Sized> QueryableField<R> {
    pub fn new<F>(default: F) -> Self
    where
        F: FilterClass<R> + 'static,
    {
        Self {
            default: Box::new(default),
            operators: BTreeMap::new(),
        }
    }

    pub fn add_operator<F>(&mut self, name: &str, filter_class: F)
    where
        F: FilterClass<R> + 'static,
    {
        self.operators
            .insert(name.to_string(), Box::new(filter_class));
    }

    pub fn create_filter(
        &self,
        operator: Option<&str>,
        rhs: &str,
    ) -> Result<Box<dyn Filter<R>>, FilterError> {
        if let Some(operator) = operator {
            self.operators
                .get(operator)
                .ok_or_else(|| FilterError::NoOperator(operator.to_string()))?
                .instantiate(rhs)
        } else {
            self.default.instantiate(rhs)
        }
    }
}

pub struct QueryableRecord<R: ?Sized> {
    // map from field names to supported operators
    fields: BTreeMap<String, QueryableField<R>>,
}

impl<R: ?Sized> Default for QueryableRecord<R> {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: ?Sized> QueryableRecord<R> {
    pub fn new() -> Self {
        Self {
            fields: BTreeMap::new(),
        }
    }

    pub fn add_field(&mut self, name: &str, q: QueryableField<R>) {
        self.fields.insert(name.to_string(), q);
    }

    pub fn create_filter(
        &self,
        field: &str,
        operator: Option<&str>,
        rhs: &str,
    ) -> Result<Box<dyn Filter<R>>, FilterError> {
        self.fields
            .get(field)
            .ok_or_else(|| FilterError::NoField(field.to_string()))?
            .create_filter(operator, rhs)
    }
}

//#[cfg(Test)]
mod tests {
    use super::*;

    use derive_queryable::Queryable;

    struct MyRecord<T> {
        string_field: String,
        int_field: i32,
        foo: T,
    }

    #[derive(Clone)]
    struct MyStringField;

    impl<T> Field<MyRecord<T>> for MyStringField
    where
        T: Clone,
    {
        type Value = String;
        fn value<'a>(&self, data: &'a MyRecord<T>) -> &'a String {
            &data.string_field
        }
    }

    #[derive(Clone)]
    struct MyIntField;

    impl<T> Field<MyRecord<T>> for MyIntField
    where
        T: Clone,
    {
        type Value = i32;
        fn value<'a>(&self, data: &'a MyRecord<T>) -> &'a i32 {
            &data.int_field
        }
    }

    #[derive(Clone)]
    struct MyTField;

    impl<T> Field<MyRecord<T>> for MyTField
    where
        T: Clone,
    {
        type Value = T;
        fn value<'a>(&self, data: &'a MyRecord<T>) -> &'a T {
            &data.foo
        }
    }

    impl<T> Queryable for MyRecord<T>
    where
        T: Clone + Eq + FromStr + 'static,
        <T as FromStr>::Err: std::error::Error + Debug + Send + Sync,
    {
        fn create_metadata() -> QueryableRecord<Self> {
            let mut qr = QueryableRecord::new();
            let qf1 =
                QueryableField::new(FilterClassImpl::new(MyStringField, crate::operators::Eq));
            let qf2 = QueryableField::new(FilterClassImpl::new(MyIntField, crate::operators::Eq));
            let qf3 = QueryableField::new(FilterClassImpl::new(MyTField, crate::operators::Eq));
            qr.add_field("string_field", qf1);
            qr.add_field("int_field", qf2);
            qr.add_field("foo", qf3);
            qr
        }
    }

    #[test]
    fn basic() {
        let r = MyRecord {
            string_field: "test".to_string(),
            int_field: 0,
            foo: "hello",
        };
        let sfield = MyStringField;
        let opcls = crate::operators::Eq;
        let op = opcls.instantiate("test").unwrap();
        let filter = FilterImpl::new(sfield, op);
        assert!(filter.filter_one(&r));
    }

    #[test]
    fn generic() {
        let r = MyRecord {
            string_field: "test".to_string(),
            int_field: 0,
            foo: 5,
        };
        let sfield = MyTField;
        let opcls = crate::operators::Eq;
        let op = opcls.instantiate("5").unwrap();
        let filter = FilterImpl::new(sfield, op);
        assert!(filter.filter_one(&r));
    }

    #[test]
    fn records() {
        let r = MyRecord {
            string_field: "test".to_string(),
            int_field: 0,
            foo: 0,
        };
        let mut qr = QueryableRecord::new();
        let qf1 = QueryableField::new(FilterClassImpl::new(MyStringField, crate::operators::Eq));
        qr.add_field("string_field", qf1);

        let filter = qr.create_filter("string_field", None, "test").unwrap();
        assert!(filter.filter_one(&r));
    }

    #[test]
    fn using_trait() {
        let r = MyRecord {
            string_field: "test3".to_string(),
            int_field: 4,
            foo: "hi".to_string(),
        };
        let qr = MyRecord::create_metadata();

        let filter = qr.create_filter("int_field", None, "4").unwrap();
        assert!(filter.filter_one(&r));
    }

    /* What's missing here are:
       - attributes to rename fields
       - attributes to select operators, or groups of operators per field
    */
    #[derive(Queryable)]
    struct MyRecord2<T>
    where
        T: Eq + FromStr + 'static,
        <T as FromStr>::Err: Debug + std::error::Error + Sync + Send + 'static,
    {
        #[django(rename = "MYSTRING")]
        string_field: String,
        int_field: i32,
        #[django(rename="bar", op(in=crate::operators::In))]
        foo: T,
    }

    #[test]
    fn using_macro() {
        let r = MyRecord2 {
            string_field: "test3".to_string(),
            int_field: 4,
            foo: 11,
        };
        let qr = MyRecord2::create_metadata();

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
        let filter = MyRecord2::create_filter_from_query_pair("int_field", "4").unwrap();
        assert!(filter.filter_one(&r));
        assert!(!filter.filter_one(&r2));

        let filter = MyRecord2::create_filter_from_query("bar=11").unwrap();
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
        let filter = MyRecord2::create_filter_from_query("bar__in=1,2,11").unwrap();
        assert!(filter.filter_one(&r));
        assert!(!filter.filter_one(&r2));
        assert!(filter.filter_one(&r3));
    }

    #[derive(Queryable)]
    struct MyRecord3<T>
    where
        T: Eq + FromStr + 'static,
        <T as FromStr>::Err: Debug + std::error::Error + Sync + Send + 'static,
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
        let filter = MyRecord3::create_filter_from_query("bar__in=1,2,11").unwrap();
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
        let filter = MyRecord3::create_filter_from_query("int_field=1,4").unwrap();
        assert!(filter.filter_one(&r));
        assert!(!filter.filter_one(&r2));
        assert!(filter.filter_one(&r3));
    }

    #[derive(Queryable)]
    struct MyRecord4<T>
    where
        T: Eq + FromStr + 'static,
        <T as FromStr>::Err: Debug + std::error::Error + Sync + Send + 'static,
    {
        #[django(
            rename = "MYSTRING",
            op(contains),
            op(icontains),
            op(startswith),
            op(endswith)
        )]
        string_field: String,
        #[django(default_fun = crate::operators::In, op(eq,lt,lte,gt,gte=crate::operators::GreaterEq))]
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
        let filter = MyRecord4::create_filter_from_query("int_field=1,4").unwrap();
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
        let filter = MyRecord4::create_filter_from_query("int_field__eq=1").unwrap();
        assert!(!filter.filter_one(&r));
        assert!(!filter.filter_one(&r2));
        assert!(filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("int_field__lt=1").unwrap();
        assert!(!filter.filter_one(&r));
        assert!(filter.filter_one(&r2));
        assert!(!filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("int_field__lte=1").unwrap();
        assert!(!filter.filter_one(&r));
        assert!(filter.filter_one(&r2));
        assert!(filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("int_field__gt=1").unwrap();
        assert!(filter.filter_one(&r));
        assert!(!filter.filter_one(&r2));
        assert!(!filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("int_field__gte=1").unwrap();
        assert!(filter.filter_one(&r));
        assert!(!filter.filter_one(&r2));
        assert!(filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("MYSTRING__contains=est").unwrap();
        assert!(filter.filter_one(&r));
        assert!(filter.filter_one(&r2));
        assert!(!filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("MYSTRING__icontains=EST").unwrap();
        assert!(filter.filter_one(&r));
        assert!(filter.filter_one(&r2));
        assert!(filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("MYSTRING__startswith=tes").unwrap();
        assert!(filter.filter_one(&r));
        assert!(filter.filter_one(&r2));
        assert!(!filter.filter_one(&r3));

        let filter = MyRecord4::create_filter_from_query("MYSTRING__endswith=3").unwrap();
        assert!(filter.filter_one(&r));
        assert!(!filter.filter_one(&r2));
        assert!(filter.filter_one(&r3));
    }
}
