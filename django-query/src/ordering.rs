/*
  A Sorter object for records of type R
  Recurses only onto primary keys for nested records
  Primary key can be handled by implementing Ord?
*/

use core::cmp::Ordering;

use std::collections::BTreeMap;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum SorterError {
    #[error("cannot sort by expression '{0}'")]
    NoSort(String)
}

pub trait Field<R>: Clone {
    type Value;
    fn value<'a>(&self, data: &'a R) -> &'a Self::Value;
}

pub trait Sorter<R> {
    fn compare(&self, a: &R, b: &R) -> Ordering;
    
    fn sort_vec(&self, vec: &mut Vec<R>) {
        vec.sort_by(|x,y| self.compare(x,y))
    }
}

pub struct SorterImpl<F> {
    field: F
}

impl<F> SorterImpl<F> {
    pub fn new(field: F) -> Self {
        Self { field }
    }
}

impl<R,F,T> Sorter<R> for SorterImpl<F>
where
    F: Field<R, Value=T>,
    T: Ord
{
    fn compare(&self, a: &R, b: &R) -> Ordering {
        self.field.value(a).cmp(self.field.value(b))
    }
}

pub struct Reverser<S> {
    inner: S
}

impl<S> Reverser<S> {
    pub fn new(inner: S) -> Self {
        Self { inner }
    }
}

impl<R,S> Sorter<R> for Reverser<S>
where
    S: Sorter<R>
{
    fn compare(&self, a: &R, b: &R) -> Ordering {
        self.inner.compare(a,b).reverse()
    }
}

pub trait SorterClass<R> {
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R>>;
}

pub struct SorterClassImpl<F> {
    field: F
}

impl<F> SorterClassImpl<F> {
    pub fn new(field: F) -> Self {
        Self { field }
    }
}

impl<R,F,T> SorterClass<R> for SorterClassImpl<F>
where
    F: Field<R, Value=T> + 'static,
    T: Ord
{
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R>> {
        if reverse {
            Box::new( Reverser::new(SorterImpl::new(self.field.clone())) )
        } else {
            Box::new( SorterImpl::new(self.field.clone()) )
        }
    }
}

pub trait SortVisitor<R> {
    fn visit_sort<F, T>(&mut self, name: &str, field: &F)
    where
        F: Field<R, Value=T> + 'static,
        T: Ord;
    fn visit_key_sort<F, G, T, U>(&mut self, name: &str, field: &F, primary_key: &G)
    where
        F: Field<R, Value=T> + 'static,
        G: Field<T, Value=U> + 'static,
        U: Ord,
        T: 'static;
}

pub trait Sortable {
    fn accept_visitor<V: SortVisitor<Self>>(visitor: &mut V) where Self: Sized;
}

pub struct SortableRecord<R> {
    sorts: BTreeMap<String, Box<dyn SorterClass<R>>>
}

impl<R: Sortable> SortableRecord<R> {
    pub fn new() -> Self {
        let mut res = Self {
            sorts: BTreeMap::new()
        };
        R::accept_visitor(&mut res);
        res
    }
    
    pub fn create_sort(&self, expr: &str) -> Result<Box<dyn Sorter<R>>, SorterError> {
        if expr.starts_with('-') {
            Ok(self.sorts
                .get(&expr[1..])
                .ok_or_else(|| SorterError::NoSort(expr.to_string()))?
                .instantiate(true))
        } else {
            Ok(self.sorts
                .get(expr)
                .ok_or_else(|| SorterError::NoSort(expr.to_string()))?
                .instantiate(false))
        }
    }
}    

impl<R: Sortable> SortVisitor<R> for SortableRecord<R> {
    fn visit_sort<F, T>(&mut self, name: &str, field: &F)
    where
        F: Field<R, Value=T> + 'static,
        T: Ord
    {
        self.sorts.insert(name.to_string(), Box::new( SorterClassImpl::new(field.clone()) ));
    }

    fn visit_key_sort<F, G, T, U>(&mut self, name: &str, field: &F, primary_key: &G)
    where
        F: Field<R, Value=T> + 'static,
        G: Field<T, Value=U> + 'static,
        U: Ord,
        T: 'static
    {
        self.sorts.insert(
            name.to_string(),
            Box::new(
                SorterClassImpl::new(
                    NestedField {
                        outer: field.clone(),
                        inner: primary_key.clone()
                    }
                )
            )
        );
    }
}

pub struct NestedField<F,G> {
    outer: F,
    inner: G
}

impl<F,G> Clone for NestedField<F,G>
where
    F: Clone,
    G: Clone
{
    fn clone(&self) -> Self {
        NestedField {
            inner: self.inner.clone(),
            outer: self.outer.clone()
        }
    }
}

impl<F,G,R,T,U> Field<R> for NestedField<F,G>
where
    F: Field<R,Value=T>,
    G: Field<T,Value=U>,
    T: 'static
{
    type Value = U;
    fn value<'a>(&self, data: &'a R) -> &'a U {
        self.inner.value(self.outer.value(data))
    }
}
