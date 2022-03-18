use std::collections::BTreeMap;
use std::fmt::Debug;
//use std::str::FromStr;

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

pub trait Operable {
    type Base: Operable;
    fn apply<O: Operator<Self::Base>>(&self, op: &O) -> bool;
}

impl<T> Operable for Vec<T>
where
    T: Operable
{
    type Base = <T as Operable>::Base;
    fn apply<O: Operator<Self::Base>>(&self, op: &O) -> bool {
        if self.is_empty() {
            op.empty_collection()
        } else {
            self.into_iter().any(|x| x.apply(op))
        }
    }
}

impl<T> Operable for Option<T>
where
    T: Operable
{
    type Base = <T as Operable>::Base;
    fn apply<O: Operator<Self::Base>>(&self, op: &O) -> bool {
        if let Some(x) = self {
            x.apply(op)
        } else {
            op.null_option()
        }
    }
}

pub trait Field<R>: Clone {
    type Value;
    fn apply<O: Operator<Self::Value>>(&self, op: &O, data: &R) -> bool;
}

pub trait Member<R>: Clone {
    type Value: Operable;
    fn apply<O: Operator<<Self::Value as Operable>::Base>>(&self, op: &O, data: &R) -> bool;
    fn accept_visitor<V: MemberVisitor<Self, R, Self::Value>>(&self, visitor: &mut V);
}

pub trait MemberVisitor<F, R, T>
where
    F: Member<R, Value=T> + Clone,
{
    fn visit_operator<O>(&mut self, name: &str, f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static, 
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable;
}

pub trait Record<R> {
    fn accept_visitor<V: RecordVisitor<R>>(&self, visitor: &mut V) where Self: Sized;
}

pub trait RecordVisitor<R> {
    fn visit_member<F,O,T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<R, Value=T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable;

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value=U> + Clone + 'static,
        T: Record<U> + 'static,
        U: 'static;
}

struct FieldNestingOperator<'a, O, S, F> {
    op: &'a O,
    inner: F,
    _marker: core::marker::PhantomData<S>
}

impl<'a,T,O,F,S> Operator<S> for FieldNestingOperator<'a,O,S,F>
where
    O: Operator<T>,
    F: Field<S, Value=T>
{
    fn apply(&self, value: &S) -> bool {
        self.inner.apply(self.op, value)
    }
}       

struct MemberNestingOperator<'a, O, S, F> {
    op: &'a O,
    inner: F,
    _marker: core::marker::PhantomData<S>
}

impl<'a,T,O,F,S> Operator<S> for MemberNestingOperator<'a,O,S,F>
where
    O: Operator<<T as Operable>::Base>,
    F: Member<S, Value=T>,
    T: Operable
{
    fn apply(&self, value: &S) -> bool {
        self.inner.apply(self.op, value)
    }
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
    S: 'static,
    T: Operable
{
    type Value = T;
    fn apply<O: Operator<Self::Value>>(&'_ self, op: &O, data: &R) -> bool {
        self.outer_field.apply(&FieldNestingOperator { op, inner: self.inner_field.clone(), _marker: Default::default() }, data)
    }
}

impl<F, G, R, S, T> Member<R> for NestedField<F, G>
where
    F: Field<R, Value=S> + 'static,
    G: Member<S, Value=T>,
    S: 'static,
    T: Operable
{
    type Value = T;
    fn apply<O: Operator<<Self::Value as Operable>::Base>>(&self, op: &O, data: &R) -> bool {
        self.outer_field.apply(&MemberNestingOperator { op, inner: self.inner_field.clone(), _marker: Default::default() }, data)
    }
    
    fn accept_visitor<V: MemberVisitor<Self, R, <Self as Member<R>>::Value>>(&self, visitor: &mut V) {
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
        G: Member<R, Value=T> + 'static,
        T: Operable,
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
        G: Member<R, Value=T> + 'static,
        T: Operable,
        R: 'static,
    {
        NestedField {
            outer_field: self.outer_field.clone(),
            inner_field
        }
    }
}

pub trait Operator<T> {
    fn apply(&self, value: &T) -> bool;
    fn empty_collection(&self) -> bool {
        false
    }
    fn null_option(&self) -> bool {
        false
    }
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
    F: Member<R, Value = T>,
    O: Operator<<T as Operable>::Base>,
    T: Operable
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
    F: Member<R, Value = T> + Clone + 'static,
    O: OperatorClass<<T as Operable>::Base>,
    <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
    T: Operable
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
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable
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
    T: Operable
{
    fn visit_operator<O>(&mut self, name: &str, f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static
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
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable
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
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable
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
    S: 'static,
    T: Operable
{
    fn visit_operator<O>(&mut self, name: &str, _f: &G, op: O)
    where
        G: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static, 
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

struct IterableField<F> {
    inner_field: F
}

impl<F> Clone for IterableField<F>
where
    F: Clone
{
    fn clone(&self) -> Self {
        IterableField {
            inner_field: self.inner_field.clone()
        }
    }
}

impl<F, R, T, I> Field<I> for IterableField<F>
where
    F: Field<R, Value=T>,
    R: 'static,
    for<'i> &'i I: IntoIterator<Item=&'i R>
{
    type Value = T;
    fn apply<O: Operator<T>>(&'_ self, op: &O, data: &I) -> bool {
        data.into_iter().any(|x| self.inner_field.apply(op, &x))
    }
}

impl<F, R, T, I> Member<I> for IterableField<F>
where
    F: Member<R, Value=T> + 'static,
    R: 'static,
    for <'i> &'i I: IntoIterator<Item=&'i R>,
    T: Operable
{
    type Value = T;
    fn apply<O: Operator<<T as Operable>::Base>>(&'_ self, op: &O, data: &I) -> bool {
        data.into_iter().any(|x| self.inner_field.apply(op, &x))
    }
    fn accept_visitor<V: MemberVisitor<Self, I, <Self as Member<I>>::Value>>(&self, visitor: &mut V) {
        let mut n = IterableMemberVisitor {
            parent: visitor,
            field: self,
            _marker: Default::default()
        };
        self.inner_field.accept_visitor(&mut n);
    }
}

struct IterableMemberVisitor<'a, F, I, P> {
    parent: &'a mut P,
    field: &'a IterableField<F>,
    _marker: core::marker::PhantomData<I>
}

impl<'a, F, P, R, T, I> MemberVisitor<F, R, T> for IterableMemberVisitor<'a, F, I, P>
where
    P: MemberVisitor<IterableField<F>, I, T>,
    F: Member<R, Value=T> + 'static,
    R: 'static,
    for<'i> &'i I: IntoIterator<Item=&'i R>,
    T: Operable
    
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static, 
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

pub struct IterableRecord;

impl<R, I> Record<I> for IterableRecord
where
    R: Queryable + 'static,
    for<'i> &'i I: IntoIterator<Item=&'i R>
{
    fn accept_visitor<V: RecordVisitor<I>>(&self, visitor: &mut V)
    where
        Self: Sized
    {
        let mut n = IterableRecordVisitor {
            parent: visitor,
            _marker: Default::default()
        };
        R::get_meta().accept_visitor(&mut n);
    }
}

struct IterableRecordVisitor<'a, P, I> {
    parent: &'a mut P,
    _marker: core::marker::PhantomData<I>
}

impl<'a, P, R, I> RecordVisitor<R> for IterableRecordVisitor<'a, P, I>
where
    P: RecordVisitor<I>,
    R: 'static,
    for<'i> &'i I: IntoIterator<Item=&'i R>
{
    fn visit_member<F,O,T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<R, Value=T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable
    {
        self.parent.visit_member(name, &IterableField { inner_field: field.clone() }, defop);
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value=U> + Clone + 'static,
        T: Record<U> + 'static,
        U: 'static
    {
        self.parent.visit_record(name, &IterableField { inner_field: field.clone() }, inner_record)
    }
}

struct PrintingVisitor {
    prefix: Option<String>
}

impl<R> RecordVisitor<R> for PrintingVisitor {
    fn visit_member<F,O,T>(&mut self, name: &str, _field: &F, _defop: O)
    where
        F: Member<R, Value=T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable
    {
        let name = if let Some(ref prefix) = self.prefix {
            format!("{}__{}", prefix, name)
        } else {
            name.to_string()
        };
        println!("Visited {}", name);
    }
    fn visit_record<F, T, U>(&mut self, name: &str, _field: &F, inner_record: &T)
    where
        F: Field<R, Value=U> + Clone + 'static,
        T: Record<U> + 'static,
        U: 'static
    {
        let new_prefix = if let Some(ref prefix) = self.prefix {
            format!("{}__{}", prefix, name)
        } else {
            name.to_string()
        };
        let mut pv = PrintingVisitor { prefix: Some(new_prefix) };
        inner_record.accept_visitor(&mut pv);
    }
}

pub fn print_queryable<Q: Queryable>() {
    let mut pv = PrintingVisitor { prefix: None };
    Q::get_meta().accept_visitor(&mut pv)
}

impl<T, I> Queryable for I
where
    T: Queryable + 'static,
    for<'i> &'i I: IntoIterator<Item=&'i T>
{
    type Meta = IterableRecord;
    fn get_meta() -> Self::Meta {
        IterableRecord
    }
}
