//! # Create sort orders for Rust objects from query URLs
//!
//! Django queries can contain an `ordering` expression, which gives the
//! fields of the object that should be used to determine
//! the relative position of objects in the result table.
//! Not all fields are sortable, but each sortable field
//! provides one natural ordering.
//!
//! For fields of structured type, the natural ordering by that field
//! can be taken from the natural ordering of one of its own fields,
//! but this relationship is fixed. This is the only form of nesting
//! present in ordering expressions.
//!
//! The expressions themselves are a comma-separated list of fields in
//! priority order. Each field can be optional prefixed with a `-` to
//! indicate the reverse ordering. So for example `"ordering=a,-b"`,
//! means sort by the `a` field, and for ties use the reverse ordering
//! of the `b` field
//!
//! # Overview
//!
//! The main trait in this module is [`Sortable`] which has an
//! associated derive macro [`macro@Sortable`]. Deriving [`Sortable`]
//! means the type can describe the sort orders it supports. An [`OrderingSet`]
//! can be constructed for any [`Sortable`] type, and can parse ordering
//! expressions from django-style queries.
//!
//! Example:
//!
//! ```rust
//! use django_query::sorting::{Sortable, OrderingSet};
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
//!
//! Because [`Sorter`] objects produced by parsing are boxed, there is
//! virtual function call overhead when using them. This can be
//! minimised by using the provided [`sort_vec`](Sorter::sort_vec)
//! method to sort an entire [`Vec`] at a time, instead of testing
//! pairs individually with [`compare`](Sorter::compare).

use core::cmp::Ordering;
use core::ops::Deref;

use std::sync::Arc;

use thiserror::Error;

/// Errors produced by sorting.
#[derive(Debug, Error)]
pub enum SorterError {
    /// The given sort expression is invalid.
    #[error("cannot sort by expression '{0}'")]
    NoSort(String),
}

/// A [`Sorter`] defined for all types that are [`Ord`].
pub struct Compare;

impl<T: Ord> Sorter<T> for Compare {
    fn compare(&self, a: &T, b: &T) -> Ordering {
        a.cmp(b)
    }
}

/// A [`Sorter`] defined for all types that are [`Ord`], which reverses
/// the natural order.
pub struct ReverseCompare;

impl<T: Ord> Sorter<T> for ReverseCompare {
    fn compare(&self, a: &T, b: &T) -> Ordering {
        b.cmp(a)
    }
}

/// The [`SorterClass`] for [`Compare`] and [`ReverseCompare`].
#[derive(Clone)]
pub struct CompareClass;

impl<'s, T: Ord> SorterClass<'s, T> for CompareClass {
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<T> + 's> {
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

/// Apply a [`Sorter`] to a member of a type.
///
/// By asking the type to apply the operator, we can avoid cloning
/// potentially large objects in more complex cases of nesting.  If we
/// used [`Accessor`] directly for everything, which is a much simpler
/// definition, there are some constructs that we cannot handle
/// efficiently, because of nested comparisons within members of type
/// [`Option`], for example (i.e. in the case that a `Foo` is to be
/// sorted by the `a` member of a contained object of type
/// `Option<Bar>`.
pub trait Field<R>: Clone {
    type Value;
    /// Extract whichever member this [Field] obtains from `R`, from
    /// both `a` and `b` and apply `op` to the results.
    fn apply_sorter<V: Sorter<Self::Value>>(&self, op: &V, a: &R, b: &R) -> Ordering;
}

/// Return a particular member of an instance by reference.
///
/// When [`Accessor`] is combined with the marker trait [`ReferenceField`], then
/// [`Field`] is automatically derived.
///
/// Example:
/// ```rust
/// use django_query::sorting::{Accessor, Field, Compare, ReferenceField};
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

/// A marker for an [`Accessor`] that can act as a field.
///
/// Not all types of [`Field`] can be defined as a simple pass
/// through, based on [`Accessor`].  Some fields need to create
/// intermediate values, for example fields nested inside [`Option`]
/// typed members of a containing structure. This marker indicates
/// that the implementation of [`Accessor`] can be used for this type
/// to automatically derive [`Field`].
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

/// Compare two values of another type `R`.
///
/// While [`Ord`] is a sensible trait for types that can have only one
/// ordering, it needs to be generalised when a type can have multiple
/// orderings according to some parameterisation. [`Sorter`] is just
/// that generalisation.
pub trait Sorter<R> {
    /// Compare two elements, returning an [`Ordering`].
    ///
    /// This is essentially the equivalent of implementing
    /// [`cmp`](core::cmp::Ord::cmp) for another type.
    fn compare(&self, a: &R, b: &R) -> Ordering;

    /// Sort a [`Vec`] in place.
    fn sort_vec(&self, vec: &mut Vec<R>) {
        vec.sort_by(|x, y| self.compare(x, y))
    }

    /// Sort a [`Vec`] of references in place.
    fn sort_ref_vec(&self, vec: &mut Vec<&R>) {
        vec.sort_by(|x, y| self.compare(x, y))
    }
}

#[doc(hidden)]
pub struct SorterImpl<F, S> {
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

/// Make a type-elided [`Sorter`].
///
/// Instances of [`SorterClass`] can be stored in an [`OrderingSet`]
/// and used to later instantiate [`Sorter`] instances.
pub trait SorterClass<'s, R> {
    /// Create a [`Sorter`], optionally reversing the order.
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R> + 's>;
}

/// Make a typed [`Sorter`].
///
/// This trait is used for any underlying [`Sorter`] that wants to be
/// used directly on a field, for example as a replacement for
/// [`Compare`]. It does not incur virtual function overhead or
/// storage overhead, but it also cannot be stored in collections for
/// a given type.
pub trait SorterTypedClass<R>: Clone {
    type Sorter: Sorter<R>;
    /// Create a new [`Sorter`].
    fn instantiate(&self) -> Self::Sorter;
}

#[derive(Clone)]
#[doc(hidden)]
pub struct SorterClassImpl<F, S> {
    field: F,
    sorter: S,
}

impl<F, S> SorterClassImpl<F, S> {
    pub fn new(field: F, sorter: S) -> Self {
        Self { field, sorter }
    }
}

impl<'s, R, F, T, S> SorterClass<'s, R> for SorterClassImpl<F, S>
where
    F: Field<R, Value = T> + 's,
    S: SorterTypedClass<T>,
    <S as SorterTypedClass<T>>::Sorter: 's,
{
    fn instantiate(&self, reverse: bool) -> Box<dyn Sorter<R> + 's> {
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

impl<'s, R, F, T, S> SorterTypedClass<R> for SorterClassImpl<F, S>
where
    F: Field<R, Value = T> + 's,
    S: SorterTypedClass<T>,
{
    type Sorter = SorterImpl<F, <S as SorterTypedClass<T>>::Sorter>;
    fn instantiate(&self) -> Self::Sorter {
        SorterImpl::new(self.field.clone(), self.sorter.instantiate())
    }
}

/// Receive descriptions of the sort orders a type provides.
///
/// Each [`Meta`] instance will accept a [`SortVisitor`] and
/// describe to it the list of sorts it supports, using the provided
/// methods.
pub trait SortVisitor<'s> {
    type Target;
    /// Receive a basic sort on a given raw [`Field`], named `name`.
    /// The comparison operator itself is given as `sort`.
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<Self::Target, Value = T> + 's,
        S: SorterTypedClass<T> + 's,
        <S as SorterTypedClass<T>>::Sorter: 's;

    /// Receive a key sort on a member field which is itself [`Sortable`].
    ///
    /// Here, `name` is the name of the sort as usual, `field` is the
    /// accessor for the member, and `sort_key` is the name of the
    /// sort in the field's own type which should be used.
    fn visit_key_sort<F, T, M>(&mut self, name: &str, field: &F, sort_key: &str, meta: M)
    where
        F: Field<Self::Target, Value = T> + 's,
        M: Meta<'s, T>;
}

/// Metadata about sorting for a type.
pub trait Meta<'s, R> {
    /// `visitor` will receive a callback for each sort that is
    /// defined for this type.
    fn accept_visitor<V: SortVisitor<'s, Target = R>>(&self, visitor: &mut V)
    where
        Self: Sized;
}

/// Something that can describe its sort orders.
///
/// This is the central trait of this module. Something that is
/// [`Sortable`] can produce a [`Meta`] describing its sort orders. It
/// has a derive macro which will automatically create all necessary
/// supporting types using the markup on the type.
pub trait Sortable<'s>: Sized {
    /// `Meta` is the type which can describe our fields and their operators.
    type Meta: Meta<'s, Self>;
    /// `get_meta` produces an instance of our `Meta` type.
    fn get_meta() -> Self::Meta;
}

impl<'s, T> Sortable<'s> for Arc<T>
where
    T: Sortable<'s>,
{
    type Meta = ArcMeta;
    fn get_meta() -> Self::Meta {
        ArcMeta
    }
}

#[doc(hidden)]
pub struct ArcMeta;

impl<'s, T> Meta<'s, Arc<T>> for ArcMeta
where
    T: Sortable<'s>,
{
    fn accept_visitor<V: SortVisitor<'s, Target = Arc<T>>>(&self, visitor: &mut V) {
        let mut v = VisitorWrapper { parent: visitor };
        T::get_meta().accept_visitor(&mut v);
    }
}

struct VisitorWrapper<'a, P> {
    parent: &'a mut P,
}

impl<'a, 's, P, R, U> SortVisitor<'s> for VisitorWrapper<'a, P>
where
    P: SortVisitor<'s, Target = U>,
    U: Deref<Target = R>,
{
    type Target = R;
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<R, Value = T> + 's,
        S: SorterTypedClass<T> + 's,
        <S as SorterTypedClass<T>>::Sorter: 's,
    {
        self.parent.visit_sort(
            name,
            &WrappedField {
                inner: field.clone(),
            },
            sort,
        );
    }

    fn visit_key_sort<F, T, M>(&mut self, name: &str, field: &F, sort_key: &str, meta: M)
    where
        F: Field<R, Value = T> + 's,
        M: Meta<'s, T>,
    {
        self.parent.visit_key_sort(
            name,
            &WrappedField {
                inner: field.clone(),
            },
            sort_key,
            meta,
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

impl<'s, T> Sortable<'s> for Option<T>
where
    T: Sortable<'s>,
{
    type Meta = OptionMeta;

    fn get_meta() -> Self::Meta {
        OptionMeta
    }
}

#[doc(hidden)]
pub struct OptionMeta;

impl<'s, T> Meta<'s, Option<T>> for OptionMeta
where
    T: Sortable<'s>,
{
    fn accept_visitor<V: SortVisitor<'s, Target = Option<T>>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut v = OptionVisitor { parent: visitor };
        T::get_meta().accept_visitor(&mut v);
    }
}

struct OptionVisitor<'a, P> {
    parent: &'a mut P,
}

impl<'a, 's, P, R> SortVisitor<'s> for OptionVisitor<'a, P>
where
    P: SortVisitor<'s, Target = Option<R>>,
{
    type Target = R;
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<R, Value = T> + 's,
        S: SorterTypedClass<T> + 's,
        <S as SorterTypedClass<T>>::Sorter: 's,
    {
        self.parent.visit_sort(
            name,
            &OptionField {
                inner: field.clone(),
            },
            sort,
        );
    }

    fn visit_key_sort<F, T, M>(&mut self, name: &str, field: &F, sort_key: &str, meta: M)
    where
        F: Field<R, Value = T> + 's,
        M: Meta<'s, T>,
    {
        self.parent.visit_key_sort(
            name,
            &OptionField {
                inner: field.clone(),
            },
            sort_key,
            meta,
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

/// A collection of sort orders, built from a [`Sortable`] type.
pub struct OrderingSet<R> {
    _marker: core::marker::PhantomData<R>,
}

impl<'s, R: Sortable<'s>> Default for OrderingSet<R> {
    fn default() -> Self {
        Self {
            _marker: Default::default(),
        }
    }
}

impl<'s, R: Sortable<'s>> OrderingSet<R>
where
    // This unnecessary and painful bound is a result of
    // https://github.com/rust-lang/rust/issues/96243 and comes about
    // because we want StackedSorter to exist, but rust cannot
    // correctly infer its lifetime
    R: 's,
{
    /// Create a new [`OrderingSet`] for a [`Sortable`] type.
    pub fn new() -> Self {
        Default::default()
    }

    /// Parse a sort expression and return a [`Sorter`].
    ///
    /// Valid sort expressions consist of a comma-separated list of sorts, each of
    /// which may be optionally preceded by a `-` to reverse its sense.
    pub fn create_sort(&self, expr: &str) -> Result<Box<dyn Sorter<R> + 's>, SorterError> {
        let parts = expr.split(',').collect::<Vec<&str>>();
        let mut full_sort: Option<Box<dyn Sorter<R> + 's>> = None;
        for part in parts.iter().rev() {
            let part_sort = if let Some(name) = part.strip_prefix('-') {
                let mut sp = SortProcessor { name, result: None };
                R::get_meta().accept_visitor(&mut sp);

                sp.result
                    .ok_or_else(|| SorterError::NoSort(part.to_string()))?
                    .instantiate(true)
            } else {
                let mut sp = SortProcessor {
                    name: *part,
                    result: None,
                };
                R::get_meta().accept_visitor(&mut sp);

                sp.result
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

#[doc(hidden)]
pub struct SortProcessor<'s, 'q, R> {
    pub name: &'q str,
    pub result: Option<Box<dyn SorterClass<'s, R> + 's>>,
}

impl<'s, 'q, R> SortVisitor<'s> for SortProcessor<'s, 'q, R> {
    type Target = R;
    fn visit_sort<F, T, S>(&mut self, name: &str, field: &F, sort: &S)
    where
        F: Field<R, Value = T> + 's,
        S: SorterTypedClass<T> + 's,
        <S as SorterTypedClass<T>>::Sorter: 's,
    {
        if name == self.name {
            self.result = Some(Box::new(SorterClassImpl::new(field.clone(), sort.clone())))
        }
    }

    fn visit_key_sort<F, T, M>(&mut self, name: &str, field: &F, key: &str, meta: M)
    where
        F: Field<R, Value = T> + 's,
        M: Meta<'s, T>,
    {
        let mut v = KeyVisitor::new(name, key, field.clone(), self);
        meta.accept_visitor(&mut v);
    }
}

#[doc(hidden)]
pub struct KeyVisitor<'a, 'b, P, F> {
    name: &'a str,
    key: &'a str,
    field: F,
    parent: &'b mut P,
}

impl<'a, 'b, P, F> KeyVisitor<'a, 'b, P, F> {
    pub fn new(name: &'a str, key: &'a str, field: F, parent: &'b mut P) -> Self {
        Self {
            name,
            key,
            field,
            parent,
        }
    }
}

impl<'a, 'b, 's, P, G, R, S> SortVisitor<'s> for KeyVisitor<'a, 'b, P, G>
where
    P: SortVisitor<'s, Target = S>,
    G: Field<S, Value = R> + 's,
{
    type Target = R;

    fn visit_sort<F, T, Srt>(&mut self, name: &str, field: &F, sort: &Srt)
    where
        F: Field<R, Value = T> + 's,
        Srt: SorterTypedClass<T> + 's,
        <Srt as SorterTypedClass<T>>::Sorter: 's,
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

    fn visit_key_sort<F, T, M>(&mut self, name: &str, field: &F, key: &str, meta: M)
    where
        F: Field<R, Value = T> + 's,
        M: Meta<'s, T>,
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
            meta.accept_visitor(&mut v);
        }
    }
}

#[derive(Clone)]
struct NestedField<F, G> {
    outer: F,
    inner: G,
}

impl<'s, F, G, R, T, U> Field<R> for NestedField<F, G>
where
    F: Field<R, Value = T>,
    G: Field<T, Value = U>,
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

struct StackedSorter<'s, R> {
    primary: Box<dyn Sorter<R> + 's>,
    secondary: Box<dyn Sorter<R> + 's>,
}

impl<'s, R> StackedSorter<'s, R> {
    pub fn new(primary: Box<dyn Sorter<R> + 's>, secondary: Box<dyn Sorter<R> + 's>) -> Self {
        Self { primary, secondary }
    }
}

impl<'s, R> Sorter<R> for StackedSorter<'s, R> {
    fn compare(&self, a: &R, b: &R) -> Ordering {
        match self.primary.compare(a, b) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => self.secondary.compare(a, b),
        }
    }
}

// State support

/// Something that can describe its sort orders, if a context value is
/// provided.
///
/// A type implementing [`SortableWithContext`] can provide a [`Meta`]
/// which describes its sorting fields, but only when a context object
/// is provided. Note that the provided context object be stored
/// within the [`Meta`], which is the origin of the lifetime parameter
/// on that type.
///
#[cfg_attr(
    feature = "persian-rug",
    doc = r##"
This can be derived via the
[`SortableWithPersianRug`](SortableWithPersianRug)
derive macro for the case of a [`persian-rug`](::persian_rug)
type.
"##
)]
pub trait SortableWithContext<'s, A: 's>: Sized {
    /// `Meta` is the type which can describe our fields and their operators.
    type Meta: Meta<'s, Self>;
    /// `get_meta` produces an instance of our `Meta` type.
    fn get_meta(access: A) -> Self::Meta;
}

/// A collection of sort orders, built from a [`SortableWithContext`] type.
///
/// Note that the context object provided on construction may be
/// stored within the resulting object, and may also be cloned into
/// the [`Meta`] for other types.
pub struct OrderingSetWithContext<R, A> {
    _marker: core::marker::PhantomData<R>,
    access: A,
}

impl<'s, A: Clone + 's, R: SortableWithContext<'s, A>> OrderingSetWithContext<R, A>
where
    // This unnecessary and painful bound is a result of
    // https://github.com/rust-lang/rust/issues/96243 and comes about
    // because we want StackedSorter to exist, but rust cannot
    // correctly infer its lifetime
    R: 's,
{
    /// Create a new [OrderingSet] for a [Sortable] type.
    pub fn new(access: A) -> Self {
        Self {
            _marker: Default::default(),
            access,
        }
    }

    /// Parse a sort expression and return a [Sorter].
    ///
    /// Valid sort expressions consist of a comma-separated list of sorts, each of
    /// which may be optionally preceded by a `-` to reverse its sense.
    pub fn create_sort(&self, expr: &str) -> Result<Box<dyn Sorter<R> + 's>, SorterError> {
        let parts = expr.split(',').collect::<Vec<&str>>();
        let mut full_sort: Option<Box<dyn Sorter<R> + 's>> = None;
        for part in parts.iter().rev() {
            let part_sort = if let Some(name) = part.strip_prefix('-') {
                let mut sp = SortProcessor { name, result: None };
                R::get_meta(self.access.clone()).accept_visitor(&mut sp);

                sp.result
                    .ok_or_else(|| SorterError::NoSort(part.to_string()))?
                    .instantiate(true)
            } else {
                let mut sp = SortProcessor {
                    name: *part,
                    result: None,
                };

                R::get_meta(self.access.clone()).accept_visitor(&mut sp);

                sp.result
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

impl<'s, T, A> SortableWithContext<'s, A> for Option<T>
where
    T: SortableWithContext<'s, A>,
    A: 's + Clone,
{
    type Meta = OptionMetaWithContext<A>;

    fn get_meta(access: A) -> Self::Meta {
        OptionMetaWithContext { access }
    }
}

#[doc(hidden)]
pub struct OptionMetaWithContext<A> {
    access: A,
}

impl<'s, T, A> Meta<'s, Option<T>> for OptionMetaWithContext<A>
where
    A: 's + Clone,
    T: SortableWithContext<'s, A>,
{
    fn accept_visitor<V: SortVisitor<'s, Target = Option<T>>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut v = OptionVisitor { parent: visitor };
        T::get_meta(self.access.clone()).accept_visitor(&mut v);
    }
}

pub use django_query_derive::Sortable;

#[cfg(feature = "persian-rug")]
#[cfg_attr(docsrs, doc(cfg(feature = "persian-rug")))]
pub use crate::persian_rug::SortableWithPersianRug;
