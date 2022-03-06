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

pub trait Field<R>: Clone
{
    type Value;
    fn value<'a>(&'_ self, data: &'a R) -> &'a Self::Value;
}

pub trait Member<R>: Field<R> {
    fn accept_visitor<V: MemberVisitor<Self, R, <Self as Field<R>>::Value>>(&self, visitor: &mut V);
}

pub trait MemberVisitor<F, R, T>
where
    F: Member<R, Value=T> + Clone,
{
    fn visit_operator<O>(&mut self, name: &str, f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<T> + 'static, 
        <O as OperatorClass<T>>::Instance: 'static;
}

pub trait Record<R> {
    fn accept_visitor<V: RecordVisitor<R>>(&self, visitor: &mut V) where Self: Sized;
}

pub trait RecordVisitor<R> {
    fn visit_member<F,O,T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<R, Value=T> + Clone + 'static,
        O: OperatorClass<T> + 'static,
        <O as OperatorClass<T>>::Instance: 'static;

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value=U> + Clone + 'static,
        T: Record<U> + 'static,
        U: 'static;
}

pub struct NestedField<F,G> {
    outer_field: F,
    inner_field: G,
}

impl<F, G> Clone for NestedField<F,G>
where
    F: Clone,
    G: Clone
{
    fn clone(&self) -> Self {
        Self {
            outer_field: self.outer_field.clone(),
            inner_field: self.inner_field.clone()
        }
    }
}

impl<F, G, R, S, T> Field<R> for NestedField<F, G>
where
    F: Field<R, Value=S>,
    G: Field<S, Value=T>,
    S: 'static
{
    type Value = T;
    fn value<'a>(&'_ self, data: &'a R) -> &'a Self::Value {
        self.inner_field.value(self.outer_field.value(data))
    }
}

impl<F, G, R, S, T> Member<R> for NestedField<F, G>
where
    F: Field<R, Value=S> + 'static,
    G: Member<S, Value=T>,
    S: 'static
{
    fn accept_visitor<V: MemberVisitor<Self, R, <Self as Field<R>>::Value>>(&self, visitor: &mut V) {
        let mut n = NestedMemberVisitor {
            parent: visitor,
            field: self,
            _marker: Default::default(),
        };
        self.inner_field.accept_visitor(&mut n);
    }
}

pub trait Nester<R, U, F>
where
    F: Field<U, Value=R>,
{
    fn nest<G,T>(&self, nested_field: G) -> NestedField<F, G>
    where
        G: Field<R, Value=T> + 'static,
        R: 'static;
}

struct NesterImpl<F>
{
    outer_field: F
}

impl<R, U, F> Nester<R,U,F> for NesterImpl<F>
where
    F: Field<U, Value=R> + Clone,
{
    fn nest<G,T>(&self, inner_field: G) -> NestedField<F, G>
    where
        G: Field<R, Value=T> + 'static,
        R: 'static
    {
        NestedField {
            outer_field: self.outer_field.clone(),
            inner_field
        }
    }
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

pub trait FilterClass<R> {
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R>>, FilterError>;
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
        self.operator.apply(self.field.value(data))
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
    <O as OperatorClass<T>>::Instance: 'static
{
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R>>, FilterError> {
        Ok(Box::new(FilterImpl::new(
            self.field.clone(),
            self.opclass.instantiate(rhs)?,
        )))
    }
}

pub trait Queryable: Sized {
    type Meta: Record<Self>;
    fn get_meta() -> Self::Meta;
}

pub struct QueryableMember<R> {
    default: Box<dyn FilterClass<R>>,
    operators: BTreeMap<String, Box<dyn FilterClass<R>>>,
}

impl<R> QueryableMember<R> {
    pub fn new<F, O, T>(f: &F, defop: O) -> Self
    where
        F: Member<R, Value=T> + 'static,
        O: OperatorClass<T> + 'static,
        <O as OperatorClass<T>>::Instance: 'static
    {
        let mut res = Self {
            default: Box::new(FilterClassImpl::new(f.clone(), defop)),
            operators: BTreeMap::new(),
        };
        f.accept_visitor(&mut res);
        res
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

impl<F, R, T> MemberVisitor<F, R, T> for QueryableMember<R>
where
    F: Member<R, Value=T> + Clone,
{
    fn visit_operator<O>(&mut self, name: &str, f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<T> + 'static,
        <O as OperatorClass<T>>::Instance: 'static
    {
        self.operators
            .insert(name.to_string(), Box::new(FilterClassImpl::new(f.clone(), op)));
    }
}   

pub struct QueryableRecord<R> {
    // map from field names to supported operators
    fields: BTreeMap<String, QueryableMember<R>>
}

impl<R: Queryable> QueryableRecord<R> {
    pub fn new() -> Self {
        let mut res = Self {
            fields: BTreeMap::new(),
        };
        R::get_meta().accept_visitor(&mut res);
        res
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

    pub fn create_filter_from_query_pair(
        &self,
        lhs: &str,
        rhs: &str,
    ) -> Result<Box<dyn Filter<R>>, FilterError> {
        if self.fields.contains_key(lhs) {
            self.create_filter(lhs, None, rhs)
        } else {
            let query_parts = lhs.rsplitn(2, "__").collect::<Vec<_>>();
            if query_parts.len() == 2 {
                self.create_filter(query_parts[1], Some(query_parts[0]), rhs)
            } else {
                Err(FilterError::NoField(lhs.to_string()))
            }
        }
    }

    pub fn create_filter_from_query(&self, expr: &str) -> Result<Box<dyn Filter<R>>, FilterError> {
        let parts = expr.splitn(2, '=').collect::<Vec<_>>();
        if parts.len() != 2 {
            Err(FilterError::MissingEquals)
        } else {
            self.create_filter_from_query_pair(parts[0], parts[1])
        }
    }
}

impl<R> RecordVisitor<R> for QueryableRecord<R> {
    fn visit_member<F,O,T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<R, Value=T> + Clone + 'static,
        O: OperatorClass<T> + 'static,
        <O as OperatorClass<T>>::Instance: 'static,
    {
        self.fields.insert(name.to_string(), QueryableMember::new(field, defop));
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value=U> + Clone + 'static,
        T: Record<U> + 'static,
        U: 'static
    {
        let mut n = NestedRecordVisitor {
            parent: self,
            prefix: name.to_string(),
            nester: NesterImpl {
                outer_field: field.clone()
            },
            _marker: Default::default(),
        };

        inner_record.accept_visitor(&mut n);
    }
}

struct NestedRecordVisitor<'a, F, R, P>
{
    parent: &'a mut P,
    prefix: String,
    nester: NesterImpl<F>,
    _marker: std::marker::PhantomData<R>,        
}

impl<'a, R, G, P, S> RecordVisitor<S> for NestedRecordVisitor<'a, G, R, P>
where
    G: Field<R, Value=S> + Clone + 'static,
    P: RecordVisitor<R>,
    S: 'static
{
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<S, Value=T> + Clone + 'static,
        O: OperatorClass<T> + 'static,
        <O as OperatorClass<T>>::Instance: 'static
    {
        self.parent.visit_member(format!("{}__{}", self.prefix, name).as_str(), 
                                 &self.nester.nest(field.clone()), defop);
    }

    fn visit_record<F,T,U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<S, Value=U> + Clone + 'static,
        T: Record<U> + 'static,
        U: 'static
    {
        let name = format!("{}__{}", self.prefix, name);
        let mut n = NestedRecordVisitor {
            parent: self,
            prefix: name,
            nester: NesterImpl {
                outer_field: field.clone()
            },
            _marker: Default::default(),

        };
        inner_record.accept_visitor(&mut n);
    }
}

struct NestedMemberVisitor<'a, F, G, R, P> {
    parent: &'a mut P,
    field: &'a NestedField<F, G>,
    _marker: core::marker::PhantomData<R>
}

impl<'a, F, G, P, R, S, T> MemberVisitor<G, S, T> for NestedMemberVisitor<'a, F, G, R, P>
where
    P: MemberVisitor<NestedField<F,G>, R, T>,
    F: Field<R, Value=S> + 'static,
    G: Member<S, Value=T>,
    S: 'static
{
    fn visit_operator<O>(&mut self, name: &str, _f: &G, op: O)
    where
        G: 'static,
        O: OperatorClass<T> + 'static, 
        <O as OperatorClass<T>>::Instance: 'static
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

/* To handle containers, we need to do two things:
   - Vec<T> -> Vec<U> to extract each field into a Vec
     PROBLEM: We would have &Vec<T> and we get Vec<&U> instead
   - Replace operators with wrapped operators

*/

struct VecField<F> {
    inner_field: F
}

impl<F> Clone for VecField<F>
where
    F: Clone
{
    fn clone(&self) -> Self {
        VecField {
            inner_field: self.inner_field.clone()
        }
    }
}

impl<F, R, T> Field<Vec<R>> for VecField<F>
where
    F: Field<R, Value=T>,
    T: Clone
{
    type Value = Vec<T>;
    fn value<'a>(&'_ self, data: &'a Vec<R>) -> &'a Self::Value {
        &data.into_iter().map(|x| self.inner_field.value(x)).cloned().collect::<Vec<T>>()
    }
}

impl<F, R, T> Member<Vec<R>> for VecField<F>
where
    F: Member<R, Value=T> + 'static,
    T: Clone
{
    fn accept_visitor<V: MemberVisitor<Self, Vec<R>, <Self as Field<Vec<R>>>::Value>>(&self, visitor: &mut V) {
        let mut n = VecMemberVisitor {
            parent: visitor,
            field: self,
            _marker: Default::default()
        };
        self.inner_field.accept_visitor(&mut n);
    }
}

struct VecMemberVisitor<'a, F, R, P> {
    parent: &'a mut P,
    field: &'a VecField<F>,
    _marker: core::marker::PhantomData<R>
}

impl<'a, F, P, R, T> MemberVisitor<F, R, T> for VecMemberVisitor<'a, F, R, P>
where
    P: MemberVisitor<VecField<F>, Vec<R>, Vec<T>>,
    F: Member<R, Value=T> + 'static,
    T: Clone
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<T> + 'static, 
        <O as OperatorClass<T>>::Instance: 'static
    {
        self.parent.visit_operator(name, self.field, crate::operators::OperatorAny { opcls: op });
    }
}

// impl Record<Vec<T>> for VecRecord
// where
//     T: Queryable
// {
//     fn accept_visitor<V: RecordVisitor<Vec<T>>>(&self, visitor: &mut V)
//     where
//         Self: Sized
//     {
//     }
// }

// impl<T> Queryable for Vec<T>
// where
//     T: Queryable
// {
//     type Meta = VecRecord;
//     fn get_meta() -> Self::Meta {
//         VecRecord
//     }
// }
