use core::cmp::Ordering;
use core::ops::Deref;

use std::collections::BTreeMap;
use std::sync::Arc;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum SorterError {
    #[error("cannot sort by expression '{0}'")]
    NoSort(String),
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
    F: Accessor<R> + ReferenceField,
{
    type Value = <F as Accessor<R>>::Value;
    fn apply_comparison<V: Comparison<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering {
        op.compare(self.value(a), self.value(b))
    }
}

pub trait Sorter<R> {
    fn compare(&self, a: &R, b: &R) -> Ordering;

    fn sort_vec(&self, vec: &mut Vec<R>) {
        vec.sort_by(|x, y| self.compare(x, y))
    }

    fn sort_ref_vec(&self, vec: &mut Vec<&R>) {
        vec.sort_by(|x, y| self.compare(x, y))
    }
}

pub struct SorterImpl<F> {
    field: F,
}

impl<F> SorterImpl<F> {
    pub fn new(field: F) -> Self {
        Self { field }
    }
}

impl<R, F, T> Sorter<R> for SorterImpl<F>
where
    F: Field<R, Value = T>,
    T: Ord,
{
    fn compare(&self, a: &R, b: &R) -> Ordering {
        self.field.apply_comparison(&Compare, a, b)
    }
}

pub struct Reverser<S> {
    inner: S,
}

impl<S> Reverser<S> {
    pub fn new(inner: S) -> Self {
        Self { inner }
    }
}

impl<R, S> Sorter<R> for Reverser<S>
where
    S: Sorter<R>,
{
    fn compare(&self, a: &R, b: &R) -> Ordering {
        self.inner.compare(a, b).reverse()
    }
}

pub trait SorterClass<R> {
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R>>;
}

pub struct SorterClassImpl<F> {
    field: F,
}

impl<F> SorterClassImpl<F> {
    pub fn new(field: F) -> Self {
        Self { field }
    }
}

impl<R, F, T> SorterClass<R> for SorterClassImpl<F>
where
    F: Field<R, Value = T> + 'static,
    T: Ord,
{
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R>> {
        if reverse {
            Box::new(Reverser::new(SorterImpl::new(self.field.clone())))
        } else {
            Box::new(SorterImpl::new(self.field.clone()))
        }
    }
}

pub trait SortVisitor {
    type Target;
    fn visit_sort<F, T>(&mut self, name: &str, field: &F)
    where
        F: Field<Self::Target, Value = T> + 'static,
        T: Ord;

    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, sort_key: &str)
    where
        F: Field<Self::Target, Value = T> + 'static,
        T: Sortable + 'static;
}

pub trait Sortable {
    fn accept_visitor<V: SortVisitor<Target = Self>>(visitor: &mut V)
    where
        Self: Sized;
}

impl<T> Sortable for Arc<T>
where
    T: Sortable,
{
    fn accept_visitor<V: SortVisitor<Target = Self>>(visitor: &mut V) {
        let mut v = VisitorWrapper { parent: visitor };
        T::accept_visitor(&mut v);
    }
}

struct VisitorWrapper<'a, P> {
    parent: &'a mut P,
}

impl<'a, P, R, U> SortVisitor for VisitorWrapper<'a, P>
where
    P: SortVisitor<Target = U>,
    U: Deref<Target = R>,
{
    type Target = R;
    fn visit_sort<F, T>(&mut self, name: &str, field: &F)
    where
        F: Field<R, Value = T> + 'static,
        T: Ord,
    {
        self.parent.visit_sort(
            name,
            &WrappedField {
                inner: field.clone(),
            },
        );
    }

    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, sort_key: &str)
    where
        F: Field<R, Value = T> + 'static,
        T: Sortable + 'static,
    {
        self.parent.visit_key_sort(
            name,
            &WrappedField {
                inner: field.clone(),
            },
            sort_key,
        );
    }
}

#[derive(Clone)]
struct WrappedField<F> {
    inner: F,
}

impl<R, F, T, U> Field<U> for WrappedField<F>
where
    F: Field<R, Value = T>,
    U: Deref<Target = R>,
{
    type Value = T;
    fn apply_comparison<V: Comparison<Self::Value>>(&self, op: &V, a: &U, b: &U) -> Ordering {
        self.inner.apply_comparison(op, a, b)
    }
}

impl<T> Sortable for Option<T>
where
    T: Sortable,
{
    fn accept_visitor<V: SortVisitor<Target = Self>>(visitor: &mut V)
    where
        Self: Sized,
    {
        let mut v = OptionVisitor { parent: visitor };
        T::accept_visitor(&mut v);
    }
}

struct OptionVisitor<'a, P> {
    parent: &'a mut P,
}

impl<'a, P, R> SortVisitor for OptionVisitor<'a, P>
where
    P: SortVisitor<Target = Option<R>>,
{
    type Target = R;
    fn visit_sort<F, T>(&mut self, name: &str, field: &F)
    where
        F: Field<R, Value = T> + 'static,
        T: Ord,
    {
        self.parent.visit_sort(
            name,
            &OptionField {
                inner: field.clone(),
            },
        );
    }

    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, sort_key: &str)
    where
        F: Field<R, Value = T> + 'static,
        T: Sortable + 'static,
    {
        self.parent.visit_key_sort(
            name,
            &OptionField {
                inner: field.clone(),
            },
            sort_key,
        );
    }
}

#[derive(Clone)]
struct OptionField<F> {
    inner: F,
}

impl<R, F, T> Field<Option<R>> for OptionField<F>
where
    F: Field<R, Value = T>,
{
    type Value = T;
    fn apply_comparison<V: Comparison<Self::Value>>(
        &self,
        op: &V,
        a: &Option<R>,
        b: &Option<R>,
    ) -> Ordering {
        match (a.as_ref(), b.as_ref()) {
            (Some(a), Some(b)) => self.inner.apply_comparison(&OptionOp { parent: op }, a, b),
            (Some(_), None) => Ordering::Greater,
            (None, Some(_)) => Ordering::Less,
            (None, None) => Ordering::Equal,
        }
    }
}

struct OptionOp<'a, V> {
    parent: &'a V,
}

impl<'a, V, T> Comparison<T> for OptionOp<'a, V>
where
    V: Comparison<T>,
{
    fn compare(&self, a: &T, b: &T) -> Ordering {
        self.parent.compare(a, b)
    }
}

pub struct OrderingSet<R> {
    sorts: BTreeMap<String, Box<dyn SorterClass<R>>>,
}

impl<R: Sortable + 'static> Default for OrderingSet<R> {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: Sortable + 'static> OrderingSet<R> {
    pub fn new() -> Self {
        let mut res = Self {
            sorts: BTreeMap::new(),
        };
        R::accept_visitor(&mut res);
        res
    }

    pub fn create_sort(&self, expr: &str) -> Result<Box<dyn Sorter<R>>, SorterError> {
        let parts = expr.split(',').collect::<Vec<&str>>();
        let mut full_sort: Option<Box<dyn Sorter<R>>> = None;
        for part in parts.iter().rev() {
            let part_sort = if let Some(name) = part.strip_prefix('-') {
                self.sorts
                    .get(name)
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
        full_sort.ok_or_else(|| SorterError::NoSort(expr.to_string()))
    }
}

impl<R: Sortable> SortVisitor for OrderingSet<R> {
    type Target = R;
    fn visit_sort<F, T>(&mut self, name: &str, field: &F)
    where
        F: Field<R, Value = T> + 'static,
        T: Ord,
    {
        self.sorts.insert(
            name.to_string(),
            Box::new(SorterClassImpl::new(field.clone())),
        );
    }

    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, key: &str)
    where
        F: Field<R, Value = T> + 'static,
        T: Sortable + 'static,
    {
        let mut v = KeyVisitor {
            name,
            key,
            field: field.clone(),
            parent: self,
        };
        T::accept_visitor(&mut v);
    }
}

struct KeyVisitor<'a, 'b, P, F> {
    name: &'a str,
    key: &'a str,
    field: F,
    parent: &'b mut P,
}

impl<'a, 'b, P, G, R, S> SortVisitor for KeyVisitor<'a, 'b, P, G>
where
    P: SortVisitor<Target = S>,
    G: Field<S, Value = R> + 'static,
    R: 'static,
{
    type Target = R;

    fn visit_sort<F, T>(&mut self, name: &str, field: &F)
    where
        F: Field<R, Value = T> + 'static,
        T: Ord,
    {
        if name == self.key {
            self.parent.visit_sort(
                self.name,
                &NestedField {
                    outer: self.field.clone(),
                    inner: field.clone(),
                },
            );
        }
    }

    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, key: &str)
    where
        F: Field<R, Value = T> + 'static,
        T: Sortable + 'static,
    {
        if name == self.key {
            let mut v = KeyVisitor {
                name: self.name,
                key,
                field: NestedField {
                    outer: self.field.clone(),
                    inner: field.clone(),
                },
                parent: self.parent,
            };
            T::accept_visitor(&mut v);
        }
    }
}
#[derive(Clone)]
pub struct NestedField<F, G> {
    outer: F,
    inner: G,
}

impl<F, G, R, T, U> Field<R> for NestedField<F, G>
where
    F: Field<R, Value = T>,
    G: Field<T, Value = U>,
    T: 'static,
{
    type Value = U;
    fn apply_comparison<V: Comparison<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering {
        let n = NestedComparison {
            inner: &self.inner,
            op,
        };
        self.outer.apply_comparison(&n, a, b)
    }
}

struct NestedComparison<'a, 'b, F, P> {
    inner: &'a F,
    op: &'b P,
}

impl<'a, 'b, F, P, T, U> Comparison<T> for NestedComparison<'a, 'b, F, P>
where
    F: Field<T, Value = U>,
    P: Comparison<U>,
{
    fn compare(&self, a: &T, b: &T) -> Ordering {
        self.inner.apply_comparison(self.op, a, b)
    }
}

pub struct StackedSorter<R> {
    primary: Box<dyn Sorter<R>>,
    secondary: Box<dyn Sorter<R>>,
}

impl<R> StackedSorter<R> {
    pub fn new(primary: Box<dyn Sorter<R>>, secondary: Box<dyn Sorter<R>>) -> Self {
        Self { primary, secondary }
    }
}

impl<R> Sorter<R> for StackedSorter<R> {
    fn compare(&self, a: &R, b: &R) -> Ordering {
        match self.primary.compare(a, b) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => self.secondary.compare(a, b),
        }
    }
}
