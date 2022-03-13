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

pub trait Comparison<T> {
    fn compare(&self, a: &T, b: &T) -> Ordering;
}

struct Compare;

impl<T: Ord> Comparison<T> for Compare {
    fn compare(&self, a: &T, b: &T) -> Ordering {
        a.cmp(b)
    }
}

pub trait Field<R>: Clone {
    type Value;
    fn apply_comparison<V: Comparison<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering;
}

pub trait Accessor<R>: Clone {
    type Value;
    fn value<'a>(&self, data: &'a R) -> &'a Self::Value;
}

pub trait ReferenceField {}

impl<R, F> Field<R> for F
where
    F: Accessor<R> + ReferenceField
{
    type Value = <F as Accessor<R>>::Value;
    fn apply_comparison<V: Comparison<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering {
        op.compare(self.value(a), self.value(b))
    }
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
        self.field.apply_comparison(&Compare, a, b)
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

    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, sort_key: &str)
    where
        F: Field<R, Value=T> + 'static,
        T: Sortable + 'static;
}

pub trait Sortable {
    fn accept_visitor<V: SortVisitor<Self>>(visitor: &mut V) where Self: Sized;
}

// #[derive(Clone)]
// struct OptionField<F> {
//     inner: F
// }

// impl<R, F, T> Field<Option<R>> for OptionField<F>
// where
//     F: Field<R, Value=T>
// {
//     type Value = Option<T>;
//     fn apply_unop<V: UnOp<Self::Value>>(&self, op: &V, a: &Option<R>) -> V::Result {
//         if let Some(a) = a.as_ref() {
//             self.inner.apply_unop(OptionVisitor { parent: &mut op })
//         } else {
//             op.apply(&None)
//         }
//     }
//     fn apply_binop<V: BinOp<Self::Value>>(&self, op: &V, a: &Option<R>, b: &Option<R>) -> V::Result {
//         self.inner.apply_binop
//     }
// }

// struct OptionVisitor<'a, V> {
//     parent: &'a V
// }

// impl<'a, 'o, V, T> UnOp<T> for OptionVisitor<'a, V>
// where
//     V: UnOp<Option<&'o T>>
// {
//     type Result = <V as UnOp<Option<&'o T>>>::Result;
//     fn apply(&self, a: &'o T) -> Self::Result {
//         self.parent.apply(&Some(a))
//     }
// }

// impl<'a, V, T> BinOp<T> for OptionVisitor<'a, V>
// where
//     V: for<'o> BinOp<Option<&'o T>>
// {
//     type Result = V::Result;
//     fn apply(&self, a: &T) -> Self::Result {
//         self.parent.apply(Some(a))
//     }
// }

pub struct SortableRecord<R> {
    sorts: BTreeMap<String, Box<dyn SorterClass<R>>>
}

impl<R: Sortable + 'static> SortableRecord<R> {
    pub fn new() -> Self {
        let mut res = Self {
            sorts: BTreeMap::new()
        };
        R::accept_visitor(&mut res);
        res
    }

    pub fn create_sort(&self, expr: &str) -> Result<Box<dyn Sorter<R>>, SorterError> {
        let parts = expr.split(',').collect::<Vec<&str>>();
        println!("Got parts: {:?}", parts);
        let mut full_sort: Option<Box<dyn Sorter<R>>> = None;
        for part in parts.iter().rev() {
            let part_sort = if part.starts_with('-') {
                self.sorts
                    .get(&part[1..])
                    .ok_or_else(|| SorterError::NoSort(part.to_string()))?
                    .instantiate(true)
            } else {
                self.sorts
                    .get(*part)
                    .ok_or_else(|| SorterError::NoSort(part.to_string()))?
                    .instantiate(false)
            };
            full_sort = if let Some(sort) = full_sort {
                Some(Box::new(StackedSorter::new(part_sort, sort)))
            } else {
                Some(part_sort)
            };
        }
        Ok(full_sort.ok_or_else(|| SorterError::NoSort(expr.to_string()))?)
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

    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, key: &str)
    where
        F: Field<R, Value=T> + 'static,
        T: Sortable + 'static
    {
        let mut v = KeyVisitor {
            name: name,
            key: key,
            field: field.clone(),
            parent: self,
            _marker: Default::default(),
        };
        T::accept_visitor(&mut v);
    }
}

struct KeyVisitor<'a, 'b, P, F, S> {
    name: &'a str,
    key: &'a str,
    field: F,
    parent: &'b mut P,
    _marker: core::marker::PhantomData<S>,
}

impl<'a, 'b, P, G, R, S> SortVisitor<R> for KeyVisitor<'a, 'b, P, G, S>
where
    P: SortVisitor<S>,
    G: Field<S, Value=R> + 'static,
    R: 'static
{
    fn visit_sort<F,T>(&mut self, name: &str, field: &F)
    where
        F: Field<R,Value=T> + 'static,
        T: Ord
    {
        if name == self.key {
            self.parent.visit_sort(self.name, &NestedField { outer: self.field.clone(), inner: field.clone() });
        }
    }

    fn visit_key_sort<F,T>(&mut self, name: &str, field: &F, key: &str)
    where
        F: Field<R, Value=T> + 'static,
        T: Sortable + 'static
    {
        if name == self.key {
            let mut v = KeyVisitor {
                name: self.name,
                key: key,
                field: NestedField { outer: self.field.clone(), inner: field.clone() },
                parent: self.parent,
                _marker: Default::default(),
            };
            T::accept_visitor(&mut v);
        }
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
    fn apply_comparison<V: Comparison<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering {
        let n = NestedComparison { inner: &self.inner, op: op };
        self.outer.apply_comparison(&n, a, b)
    }
}

struct NestedComparison<'a, 'b, F, P> {
    inner: &'a F,
    op: &'b P
}

impl<'a,'b, F,P,T,U> Comparison<T> for NestedComparison<'a, 'b, F, P>
where
    F: Field<T, Value=U>,
    P: Comparison<U>
{
    fn compare(&self, a: &T, b: &T) -> Ordering {
        self.inner.apply_comparison(self.op, a, b)
    }
}

pub struct StackedSorter<R> {
    primary: Box<dyn Sorter<R>>,
    secondary: Box<dyn Sorter<R>>
}

impl<R> StackedSorter<R> {
    pub fn new(primary: Box<dyn Sorter<R>>, secondary: Box<dyn Sorter<R>>) -> Self {
        Self { primary, secondary }
    }
}

impl<R> Sorter<R> for StackedSorter<R>
{
    fn compare(&self, a: &R, b: &R) -> Ordering {
        match self.primary.compare(a, b) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => self.secondary.compare(a, b)
        }
    }
}
