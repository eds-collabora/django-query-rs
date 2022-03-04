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
    fn apply<O: Operator<Self::Value>>(&self, op: &O, data: &R) -> bool;
}

pub trait ScalarField<R> {
    type Value;
    fn value<'a>(&'_ self, data: &'a R) -> &'a Self::Value;
}

impl<R,T> Field<R> for T
where
    T: ScalarField<R>
{
    type Value = <T as ScalarField<R>>::Value;
    fn apply<O: Operator<Self::Value>>(&self, op: &O, data: &R) -> bool {
        op.apply(self.value(data))
    }
}

pub struct NestedField<F,G> {
    outer_field: F,
    inner_field: G,
}
    
impl<F, G, R, S, T> ScalarField<R> for NestedField<F, G>
where
    F: ScalarField<R, Value=S>,
    G: ScalarField<S, Value=T>
{
    type Value = <G as ScalarField<S>>::Value;
    fn value<'a>(&'_ self, data: &'a R) -> &'a Self::Value {
        self.inner_field.value(self.outer_field.value(data))
    }
}

/*
pub struct VectorField<F> {
    field: F
}

impl<F, R, S> Field<R> for VectorField<F>
where
    F: Field<R>,
    for<'a> &'a<F as Field<R>>::Value: IntoIterator<Item=S>,
{
    type Value = S;
    fn apply<O: Operator<Self::Value>>(&self, op: &O, data: &R) -> bool {
        self.field.value().into_iter().any(|x| op.apply(x))
    }
}
 */

const _: () = {
    struct Record {
        foo: Vec<String>,
    }
    struct VecField;
    impl Field<Record> for VecField {
        type Value = String;
        fn apply<O: Operator<Self::Value>>(&self, op: &O, data: &Record) -> bool {
            data.foo.iter().any(|x| op.apply(x))
        }
    }
};

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

pub struct FilterImpl<F, O> {
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
        self.field.apply(&self.operator, data)
    }
}

pub struct FilterClassImpl<F, O> {
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
