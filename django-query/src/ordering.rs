//! Create sort orders for Rust objects from query URLs.
//!
//! Django permits sorting by multiple fields simultaneously, and each
//! field can be in ascending or descending order. There is only one
//! natural ordering permitted however per field, although it is
//! possible to sort by foreign keys (i.e. when applied in Rust, it's
//! possible to sort a collection of objects by a common member whose
//! type is also structured).
//!
//! The main trait in this module is [Sortable] which has an
//! associated derive macro. Deriving [Sortable] means it's possible
//! to construct an [OrderingSet] for this type, which contains a
//! lookup for boxed [Sorter]s. A sorter can be used as a simple
//! comparison, paying the virtual function overhead for each
//! comparison made, or more efficiently it can be applied to an
//! entire [Vec] at a time to sort it.
//!
//! Example:
//!
//! ```rust
//! use django_query::{Sortable, OrderingSet};
//!
//! #[derive(Sortable)]
//! struct Bar {
//!   #[django(sort)]
//!   a: i32,
//!   #[django(sort)]
//!   b: i32,
//! }
//!
//! #[derive(Sortable)]
//! struct Foo {
//!   #[django(sort)]
//!   c: i32,
//!   #[django(sort("b"))]
//!   d: Bar,
//! }
//!
//! let mut foos = vec![
//!     Foo { c: 0, d: Bar { a: 1, b: 1 } },
//!     Foo { c: 1, d: Bar { a: 0, b: 0 } },
//!     Foo { c: 2, d: Bar { a: 2, b: 1 } },
//!     Foo { c: 3, d: Bar { a: 3, b: 0 } },
//! ];
//!
//! let qr = OrderingSet::<Foo>::new();
//!
//! let sort = qr.create_sort("-c").unwrap();
//! sort.sort_vec(&mut foos);
//! assert_eq!(foos[0].c, 3);
//! assert_eq!(foos[1].c, 2);
//! assert_eq!(foos[2].c, 1);
//! assert_eq!(foos[3].c, 0);
//!
//! let sort = qr.create_sort("d,c").unwrap();
//! sort.sort_vec(&mut foos);
//! assert_eq!(foos[0].c, 1);
//! assert_eq!(foos[1].c, 3);
//! assert_eq!(foos[2].c, 0);
//! assert_eq!(foos[3].c, 2);
//! ```

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

/// A [Sorter] defined for all types that are [Ord].
pub struct Compare;

impl<T: Ord> Sorter<T> for Compare {
    fn compare(&self, a: &T, b: &T) -> Ordering {
        a.cmp(b)
    }
}

/// A [Sorter] defined for all types that are [Ord], which reverses
/// the natural order.
pub struct ReverseCompare;

impl<T: Ord> Sorter<T> for ReverseCompare {
    fn compare(&self, a: &T, b: &T) -> Ordering {
        b.cmp(a)
    }
}

/// The [SorterClass] for [Compare]
#[derive(Clone)]
pub struct CompareClass;

impl<T: Ord> SorterClass<T> for CompareClass {
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<T>> {
        if reverse {
            Box::new(ReverseCompare)
        } else {
            Box::new(Compare)
        }
    }
}

impl<T: Ord> SorterTypedClass<T> for CompareClass {
    type Sorter = Compare;
    fn instantiate(&self) -> Compare {
        Compare
    }
}

/// Something that can apply a [Sorter] to a member of a type.
///
/// By asking the type to apply the operator, we can avoid cloning
/// potentially large objects in more complex cases of nesting.  If we
/// used [Accessor] directly for everything, which is a much simpler
/// definition, there are some constructs that we cannot handle
/// efficiently, because of nested comparisons within members of type
/// [Option], for example (i.e. in the case that a `Foo` is to be
/// sorted by the `a` member of a contained object of type
/// `Option<Bar>`.
pub trait Field<R>: Clone {
    type Value;
    /// Extract whichever member this [Field] obtains from `R`, from
    /// both `a` and `b` and apply `op` to the results.
    fn apply_sorter<V: Sorter<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering;
}

/// Something which returns a particular member of an instance by reference.
///
/// When `Accessor` is combined with the marker trait `ReferenceField`, you will automatically
/// get a derivation of `Field`.
///
/// Example:
/// ```rust
/// use django_query::ordering::{Accessor, Field, Compare, ReferenceField};
/// use core::cmp::Ordering;
///
/// struct Foo {
///   a: i32
/// }
///
/// #[derive(Clone)]
/// struct FooA;
///
/// impl Accessor<Foo> for FooA {
///     type Value = i32;
///     fn value<'a>(&self, data: &'a Foo) -> &'a i32 {
///        &data.a
///     }
/// }
///
/// impl ReferenceField for FooA {}
///
/// let f_a = FooA;
/// let foo1 = Foo { a: 20 };
/// let foo2 = Foo { a: 10 };
/// assert_eq!(f_a.value(&foo1), &20i32);
/// assert_eq!(f_a.apply_sorter(&Compare, &foo1, &foo2), Ordering::Greater);
/// ```
pub trait Accessor<R>: Clone {
    type Value;
    /// Return a reference to a member of `data`
    fn value<'a>(&self, data: &'a R) -> &'a Self::Value;
}

/// A marker for an [Accessor] that can act as a field.
///
/// Not all types of [Field] can be defined as a simple pass through,
/// based on [Accessor].  Some fields need to create intermediate
/// values, for example fields nested inside [Option] typed members of
/// a containing structure. This marker indicates that the
/// implementation of [Accessor] can be used for this type to
/// automatically derive [Field].
pub trait ReferenceField {}

impl<R, F> Field<R> for F
where
    F: Accessor<R> + ReferenceField,
{
    type Value = <F as Accessor<R>>::Value;
    fn apply_sorter<V: Sorter<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering {
        op.compare(self.value(a), self.value(b))
    }
}

/// Something that can compare two values of another type `T`.
///
/// While [Ord] is a sensible trait for types that can have only one
/// ordering, it needs to be generalised when a type can have multiple
/// orderings according to some parameterisation. `Sorter` is just
/// that generalisation.
pub trait Sorter<R> {
    fn compare(&self, a: &R, b: &R) -> Ordering;

    /// Sort a [Vec] in place.
    fn sort_vec(&self, vec: &mut Vec<R>) {
        vec.sort_by(|x, y| self.compare(x, y))
    }

    /// Sort a [Vec] of references in place.
    fn sort_ref_vec(&self, vec: &mut Vec<&R>) {
        vec.sort_by(|x, y| self.compare(x, y))
    }
}

struct SorterImpl<F, S> {
    field: F,
    sorter: S,
}

impl<F, S> SorterImpl<F, S> {
    pub fn new(field: F, sorter: S) -> Self {
        Self { field, sorter }
    }
}

impl<R, F, T, S> Sorter<R> for SorterImpl<F, S>
where
    F: Field<R, Value = T>,
    S: Sorter<T>,
{
    fn compare(&self, a: &R, b: &R) -> Ordering {
        self.field.apply_sorter(&self.sorter, a, b)
    }
}

struct Reverser<S> {
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

/// Something that can make a boxed [Sorter].
///
/// This is the boxed object-like trait that is stored in an
/// [OrderingSet]. When a specific sort is needed, it can be
/// used to create one.
pub trait SorterClass<R> {
    /// Create a [Sorter], optionally reversing the order.
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R>>;
}

/// Something which can make a typed [Sorter].
///
/// This trait is used for any underlying [Sorter] that wants to be
/// used directly on a field, for example as a replacement for
/// [Compare]. It does not incur virtual function overhead or storage
/// overhead, but it also cannot be stored in collections for a given
/// type.
pub trait SorterTypedClass<R>: Clone {
    type Sorter: Sorter<R>;
    fn instantiate(&self) -> Self::Sorter;
}

#[derive(Clone)]
struct SorterClassImpl<F, S> {
    field: F,
    sorter: S,
}

impl<F, S> SorterClassImpl<F, S> {
    pub fn new(field: F, sorter: S) -> Self {
        Self { field, sorter }
    }
}

impl<R, F, T, S> SorterClass<R> for SorterClassImpl<F, S>
where
    F: Field<R, Value = T> + 'static,
    S: SorterTypedClass<T>,
    <S as SorterTypedClass<T>>::Sorter: 'static,
{
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R>> {
        if reverse {
            Box::new(Reverser::new(SorterImpl::new(
                self.field.clone(),
                self.sorter.instantiate(),
            )))
        } else {
            Box::new(SorterImpl::new(
                self.field.clone(),
                self.sorter.instantiate(),
            ))
        }
    }
}

impl<R, F, T, S> SorterTypedClass<R> for SorterClassImpl<F, S>
where
    F: Field<R, Value = T> + 'static,
    S: SorterTypedClass<T>,
{
    type Sorter = SorterImpl<F, <S as SorterTypedClass<T>>::Sorter>;
    fn instantiate(&self) -> Self::Sorter {
        SorterImpl::new(self.field.clone(), self.sorter.instantiate())
    }
}

/// Something that can receive callbacks about the sort orders a type
/// provides.
///
/// Each [Sortable] instance will accept a [SortVisitor] and describe
/// to it the list of sorts it supports, using the provided methods.
pub trait SortVisitor {
    type Target;
    /// Receive a basic sort on a given raw [Field], named `name`.
    /// The comparison operator itself is given as `sort`.
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<Self::Target, Value = T> + 'static,
        S: SorterTypedClass<T> + 'static,
        <S as SorterTypedClass<T>>::Sorter: 'static;

    /// Receive a key sort on a member field which is itself [Sortable].
    ///
    /// Here, `name` is the name of the sort as usual, `field` is the
    /// accessor for the member, and `sort_key` is the name of the
    /// sort in the field's own type which should be used.
    fn visit_key_sort<F, T>(&mut self, name: &str, field: &F, sort_key: &str)
    where
        F: Field<Self::Target, Value = T> + 'static,
        T: Sortable + 'static;
}

/// Something that can be sorted.
///
/// This is the central trait of this module. It has a derive macro
/// which will automatically create all necessary supporting types
/// using the markup on the type.
pub trait Sortable {
    /// `visitor` will receive a callback for each sort that is
    /// defined for this type.
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
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<R, Value = T> + 'static,
        S: SorterTypedClass<T> + 'static,
        <S as SorterTypedClass<T>>::Sorter: 'static,
    {
        self.parent.visit_sort(
            name,
            &WrappedField {
                inner: field.clone(),
            },
            sort,
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
    fn apply_sorter<V: Sorter<Self::Value>>(&self, op: &V, a: &U, b: &U) -> Ordering {
        self.inner.apply_sorter(op, a, b)
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
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<R, Value = T> + 'static,
        S: SorterTypedClass<T> + 'static,
        <S as SorterTypedClass<T>>::Sorter: 'static,
    {
        self.parent.visit_sort(
            name,
            &OptionField {
                inner: field.clone(),
            },
            sort,
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
    fn apply_sorter<V: Sorter<Self::Value>>(
        &self,
        op: &V,
        a: &Option<R>,
        b: &Option<R>,
    ) -> Ordering {
        match (a.as_ref(), b.as_ref()) {
            (Some(a), Some(b)) => self.inner.apply_sorter(&OptionOp { parent: op }, a, b),
            (Some(_), None) => Ordering::Greater,
            (None, Some(_)) => Ordering::Less,
            (None, None) => Ordering::Equal,
        }
    }
}

struct OptionOp<'a, V> {
    parent: &'a V,
}

impl<'a, V, T> Sorter<T> for OptionOp<'a, V>
where
    V: Sorter<T>,
{
    fn compare(&self, a: &T, b: &T) -> Ordering {
        self.parent.compare(a, b)
    }
}

/// A collection of sort orders, built from a [Sortable] type.
pub struct OrderingSet<R> {
    sorts: BTreeMap<String, Box<dyn SorterClass<R>>>,
}

impl<R: Sortable + 'static> Default for OrderingSet<R> {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: Sortable + 'static> OrderingSet<R> {
    /// Create a new [OrderingSet] for a [Sortable] type.
    pub fn new() -> Self {
        let mut res = Self {
            sorts: BTreeMap::new(),
        };
        R::accept_visitor(&mut res);
        res
    }

    /// Parse a sort expression and return a [Sorter].
    ///
    /// Valid sort expressions consist of a comma-separated list of sorts, each of
    /// which may be optionally preceded by a `-` to reverse its sense.
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
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<R, Value = T> + 'static,
        S: SorterTypedClass<T> + 'static,
        <S as SorterTypedClass<T>>::Sorter: 'static,
    {
        self.sorts.insert(
            name.to_string(),
            Box::new(SorterClassImpl::new(field.clone(), sort.clone())),
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

    fn visit_sort<F, T, Srt>(&mut self, name: &str, field: &F, sort: &Srt)
    where
        F: Field<R, Value = T> + 'static,
        Srt: SorterTypedClass<T> + 'static,
        <Srt as SorterTypedClass<T>>::Sorter: 'static,
    {
        if name == self.key {
            self.parent.visit_sort(
                self.name,
                &NestedField {
                    outer: self.field.clone(),
                    inner: field.clone(),
                },
                sort,
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
struct NestedField<F, G> {
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
    fn apply_sorter<V: Sorter<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering {
        let n = NestedSorter {
            inner: &self.inner,
            op,
        };
        self.outer.apply_sorter(&n, a, b)
    }
}

struct NestedSorter<'a, 'b, F, P> {
    inner: &'a F,
    op: &'b P,
}

impl<'a, 'b, F, P, T, U> Sorter<T> for NestedSorter<'a, 'b, F, P>
where
    F: Field<T, Value = U>,
    P: Sorter<U>,
{
    fn compare(&self, a: &T, b: &T) -> Ordering {
        self.inner.apply_sorter(self.op, a, b)
    }
}

struct StackedSorter<R> {
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
