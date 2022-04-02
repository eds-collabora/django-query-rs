//! # Create filters for Rust objects from query URLs
//!
//! Django encodes database queries into URL query strings in a way
//! that can be frustrating to replicate ad-hoc. The simplest query
//! parts are things like `foo=3` which means the column `foo` must
//! have the exact value 3. More precisely, it means: "apply the
//! default operator for column `foo`, to the contents of that column
//! for each row, with a right hand side of 3, and only include the
//! row if the result is `true`. The default operator is usually
//! `exact`.
//!
//! There are three ways Django extends this simple model. Firstly,
//! it's possible to specify a different operator than the default
//! for some columns. Standard Django operators include `contains`,
//! `gt`, `lt`, `in`, `iexact` and `isnull`. These are written in
//! the query string as, for example, `foo__lt=3`, which filters on
//! `foo < 3`.
//!
//! Secondly, columns can be multivalued. If in the preceding example
//! column `foo` an array of integers, then `foo=3` would mean that
//! the array must contain 3. In general, for collection valued
//! columns, a filter is considered to pass if any individual element
//! passes.
//!
//! Thirdly, there can be relations between tables. For example, if
//! the column `foo` was actually of some complex type, drawn from a
//! different table, then it's possible to filter within `foo`. Suppose
//! the table for the type of member `foo` has a column `bar`, then
//! our query string could be `foo__bar=3` to mean "select all objects
//! whose member `foo` has a member `bar` whose value is 3".
//!
//! All of the preceding features combine, so it's possible to end up
//! with query strings like `foo__bar__baz__lt=5` with the meaning
//! (due to the structure with collections): "include objects within
//! whose collection for member `foo`, one item has a member `bar`,
//! and within that object's member `bar` one item has a member `baz`,
//! and within the collection of integers for that object's member
//! `baz` one integer is less than 5".
//!
//! ## Overview
//!
//! The main trait in this module is [`Filterable`] which has an
//! associated derive macro [`macro@Filterable`]. Deriving
//! [`Filterable`] means the type can describe the filtering it
//! supports, and the derive macro allows you to set up the filters
//! inline in the type definition.  An [`OperatorSet`] can be
//! constructed for any [`Filterable`] type, which can convert a URL
//! fragment from a django-style query string into a boxed [`Filter`]
//! object.
//!
//! Example:
//! ```rust
//! use django_query::filtering::{Filterable, OperatorSet};
//!
//! #[derive(Filterable)]
//! struct Bar {
//!   #[django(op(lt))]
//!   a: i32,
//! }
//!
//! #[derive(Filterable)]
//! struct Foo {
//!   #[django(op(lt))]
//!   a: i32,
//!   #[django(traverse)]
//!   b: Bar,
//! }
//!
//! let foo = Foo { a: 5, b: Bar { a: 0 } };
//! let qr = OperatorSet::<Foo>::new();
//! let filter = qr.create_filter_from_query("b__a__lt=1").unwrap();
//! assert!(filter.filter_one(&foo));
//! let filter = qr.create_filter_from_query("b__a__lt=0").unwrap();
//! assert!(!filter.filter_one(&foo));
//! ```
//!
//! Because [`Filter`] objects produced by parsing are boxed, there is
//! virtual function call overhead when using them. This can be
//! minimised by using the provided [`filter_vec`](Filter::filter_vec)
//! method to filter an entire [`Vec`] at a time, instead of testing
//! values individually with [`filter_one`](Filter::filter_one).
//!
//! Note that self-including types - types that have members of their
//! own type at any level of indirection - cannot be handled by this
//! crate because of its commitment to monomorphism within filters. It
//! must always be possible to write out all the valid left hand sides
//! that could occur in a filter, and this will in fact be done
//! automatically when an [`OperatorSet`] is constructed.

pub mod ops;

use core::ops::Deref;

use std::fmt::Debug;
use std::sync::Arc;

use thiserror::Error;

/// Errors produced by filtering.
#[derive(Debug, Error)]
pub enum FilterError {
    /// There is no equals sign in the query string.
    #[error("malformed query has no '='")]
    MissingEquals,
    /// An operator was requested that isn't defined for the target type.
    #[error("no such operator '{0}'")]
    NoOperator(String),
    /// The requested field is not present on the target type.
    #[error("no such field '{0}'")]
    NoField(String),
    /// Failed to instantiate a [`Filter`].
    #[error(transparent)]
    Instantiation(#[from] anyhow::Error),
}

/// A type that Django filters can work on.
///
/// Django filters are defined in terms of operators on
/// simple values, like strings and numbers. There
/// is additional complexity, however, because these simple
/// specifications can also be applied to collections and nullable
/// values.
///
/// All types that occur naturally in Django queries can be made
/// [`Operable`], but so can, for example, [`Vec<T>`] and
/// [`Option<T>`], when the `T` in question is itself
/// [`Operable`]. It's up to an [`Operable`] implementation to decide
/// how to take an operator on its base type and apply it to its
/// contents, for example by iterating over whatever contents it has.
///
/// The implementations for [`Vec<T>`] and [`Option<T>`] match the default
/// behaviour in Django.
pub trait Operable {
    /// The underlying type that operators should be applied to.
    type Base;
    /// Apply `op` across our contents.
    fn apply<O: Operator<Self::Base>>(&self, op: &O) -> bool;
}

impl<T> Operable for Vec<T>
where
    T: Operable,
{
    type Base = <T as Operable>::Base;
    fn apply<O: Operator<Self::Base>>(&self, op: &O) -> bool {
        if self.is_empty() {
            op.empty_collection()
        } else {
            self.iter().any(|x| x.apply(op))
        }
    }
}

impl<T> Operable for Option<T>
where
    T: Operable,
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

/// A wrapper for a field of any type.
///
/// A [`Field`] allows us to apply an [`Operator`] to one field of a
/// structure. You will normally generate implementations of [`Field`]
/// using the [`Filterable`] derive macro for your type. The key use
/// of [`Field`] is in traversing nested structures: whereas a
/// [`Member`] represents applying an operator to an [`Operable`]
/// value, the value of a [`Field`] can be arbitrary, and is often a
/// structured type.
pub trait Field<R>: Clone {
    /// The type an operator on this field must accept.
    type Value;
    /// Apply `op` to a particular field of `data`, and return the
    /// result.
    fn apply<O: Operator<Self::Value>>(&self, op: &O, data: &R) -> bool;
}

/// A wrapper for a field of [`Operable`] type.
///
/// A [`Member`] is a filterable field of a structure. You will
/// normally generate implementations of [`Member`] using the
/// [`Filterable`] derive macro for your type. Whereas a [`Field`]
/// provides access for nesting, a [`Member`] is visible to queries,
/// and will expose a list of supported operators to a
/// [`MemberVisitor`].
pub trait Member<'a, R>: Clone + 'a {
    /// The type of the member's data, which must be [`Operable`] so
    /// that Django filtering queries can be made against it.
    type Value: Operable;
    /// Apply an operator to the given field of `data`, returning the
    /// result.
    fn apply<O: Operator<<Self::Value as Operable>::Base>>(&self, op: &O, data: &R) -> bool;
    /// `visitor` will be called with each of the supported operator
    /// classes for this member. This is used to build up a list
    /// of supported queries in an [`OperatorSet`].
    fn accept_visitor<V: MemberVisitor<'a, Self, R, Self::Value>>(&self, visitor: &mut V);
}

/// Receive descriptions of the filters a [`Member`] supports.
///
/// A [`MemberVisitor`] can be passed to
/// [`accept_visitor`](Member::accept_visitor) on a [`Member`] to
/// receive a callback for each [`OperatorClass`] that [`Member`]
/// supports.
pub trait MemberVisitor<'a, F, R, T>
where
    F: Member<'a, R, Value = T> + Clone,
{
    /// This is called by [`Member`] for each [`OperatorClass`] it
    /// supports. Here:
    /// - `name` is the name of the operator in queries (e.g. "in")
    /// - `f` is the [`Member`] itself.
    /// - `op` is an [`OperatorClass`] with a target type matching the
    ///   `Member` type.
    fn visit_operator<O>(&mut self, name: &str, f: &F, op: O)
    where
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
        T: Operable;
}

/// A wrapper for a type that can be filtered.
///
/// A [`Meta`] holds a representation of a filterable type, similar to
/// [`Member`] for fields, but now for an entire structured type. When
/// a type is [`Filterable`] it can produce a [`Meta`] for itself, to
/// enable discovery of its members and their supported operators.
pub trait Meta<'a, R> {
    /// `visitor` will be called with each exposed member of the type.
    fn accept_visitor<V: MetaVisitor<'a, R>>(&self, visitor: &mut V)
    where
        Self: Sized;
}

/// Receive descriptions of the [`Member`]s a type contains from its [`Meta`].
///
/// A [`MetaVisitor`] can be passed to
/// [`accept_visitor`](Meta::accept_visitor) on [`Meta`] to receive a
/// callback for each of the filterable members of a type.
pub trait MetaVisitor<'a, R> {
    /// This will be called by a [`Meta`] instance for each filterable structure
    /// member which is [`Operable`]. Here:
    /// - `name` is the name of the membner, as exposed.
    /// - `member` is a [`Member`] wrapping that field.
    /// - `defop` is the default [`OperatorClass`] for that member, which
    ///    should be applied when no operator is specified (i.e. in
    ///    the `"record__field"` case as opposed to the
    ///    `"record__field__op"` case)
    fn visit_member<F, O, T>(&mut self, name: &str, member: &F, defop: O)
    where
        F: Member<'a, R, Value = T> + Clone,
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
        T: Operable;

    /// This will be called by  a [`Meta`] instance for each filterable structure field
    /// that can be traversed. This is true when the field's type is itself
    /// a structured type, and is not directly filterable. Here:
    /// - `name` is the name of the field, as exposed.
    /// - `field` is a [`Field`] wrapping that field (note that
    ///    there are no exposed operators for a [`Field`], and operators
    ///    are applied to the record type, since there could not be a single
    ///    [`Operable::Base`] type.
    /// - `inner_record` is the [`Meta`] for the field.
    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value = U> + Clone + 'a,
        T: Meta<'a, U>;
}

struct FieldNestingOperator<'a, O, S, F> {
    op: &'a O,
    inner: F,
    _marker: core::marker::PhantomData<S>,
}

impl<'a, T, O, F, S> Operator<S> for FieldNestingOperator<'a, O, S, F>
where
    O: Operator<T>,
    F: Field<S, Value = T>,
{
    fn apply(&self, value: &S) -> bool {
        self.inner.apply(self.op, value)
    }
}

struct MemberNestingOperator<'a, O, S, F> {
    op: &'a O,
    inner: F,
    _marker: core::marker::PhantomData<S>,
}

impl<'a, 'b, O, F, S> Operator<S> for MemberNestingOperator<'a, O, S, F>
where
    F: Member<'b, S>,
    O: Operator<<<F as Member<'b, S>>::Value as Operable>::Base>,
    <F as Member<'b, S>>::Value: Operable,
{
    fn apply(&self, value: &S) -> bool {
        self.inner.apply(self.op, value)
    }
}

struct NestedField<F, G> {
    outer_field: F,
    inner_field: G,
}

impl<F, G> Clone for NestedField<F, G>
where
    F: Clone,
    G: Clone,
{
    fn clone(&self) -> Self {
        Self {
            outer_field: self.outer_field.clone(),
            inner_field: self.inner_field.clone(),
        }
    }
}

impl<F, G, R, S, T> Field<R> for NestedField<F, G>
where
    F: Field<R, Value = S>,
    G: Field<S, Value = T>,
{
    type Value = T;
    fn apply<O: Operator<Self::Value>>(&'_ self, op: &O, data: &R) -> bool {
        self.outer_field.apply(
            &FieldNestingOperator {
                op,
                inner: self.inner_field.clone(),
                _marker: Default::default(),
            },
            data,
        )
    }
}

impl<'a, F, G, R, S, T> Member<'a, R> for NestedField<F, G>
where
    F: Field<R, Value = S> + 'a,
    G: Member<'a, S, Value = T>,
    T: Operable,
{
    type Value = T;
    fn apply<O: Operator<<Self::Value as Operable>::Base>>(&self, op: &O, data: &R) -> bool {
        self.outer_field.apply(
            &MemberNestingOperator {
                op,
                inner: self.inner_field.clone(),
                _marker: Default::default(),
            },
            data,
        )
    }

    fn accept_visitor<V: MemberVisitor<'a, Self, R, <Self as Member<'a, R>>::Value>>(
        &self,
        visitor: &mut V,
    ) {
        let mut n = NestedMemberVisitor {
            parent: visitor,
            field: self,
            _marker: Default::default(),
        };
        self.inner_field.accept_visitor(&mut n);
    }
}

trait Nester<'a, R, U, F>
where
    F: Field<U, Value = R> + 'a,
{
    fn nest_member<G, T>(&self, nested_field: G) -> NestedField<F, G>
    where
        G: Member<'a, R, Value = T>,
        T: Operable;

    fn nest_field<G, T>(&self, inner_field: G) -> NestedField<F, G>
    where
        G: Field<R, Value = T> + 'a;
}

struct NesterImpl<F> {
    outer_field: F,
}

impl<'a, R, U, F> Nester<'a, R, U, F> for NesterImpl<F>
where
    F: Field<U, Value = R> + Clone + 'a,
{
    fn nest_member<G, T>(&self, inner_field: G) -> NestedField<F, G>
    where
        G: Member<'a, R, Value = T>,
        T: Operable,
    {
        NestedField {
            outer_field: self.outer_field.clone(),
            inner_field,
        }
    }

    fn nest_field<G, T>(&self, inner_field: G) -> NestedField<F, G>
    where
        G: Field<R, Value = T> + 'a,
    {
        NestedField {
            outer_field: self.outer_field.clone(),
            inner_field,
        }
    }
}

/// Take a single value and produce a true/false result.
///
/// Operators are generally applied by [`Operable`] types, so the
/// operators themselves can be naive with respect to things like
/// [`Option<T>`] and collections. For operators that need to know about
/// special cases, there are the inelegant get-out functions
/// [`empty_collection`](Operator::empty_collection) and
/// [`null_option`](Operator::null_option) which can be
/// implemented. Both return `false` by default.
///
/// The standard Django operators are in the
/// [`ops`] module. Examples are
/// [`exact`](ops::ExactImpl),
/// [`in`](ops::InImpl),
/// [`contains`](ops::ContainsImpl) and so forth.
pub trait Operator<T> {
    /// Apply this operator a single value, producing a true/false
    /// result.
    fn apply(&self, value: &T) -> bool;
    /// Return a value for this operator when applied to an empty
    /// collection.  Implicitly, operators are distributed over
    /// collections with `any` semantics, so that any `true` result
    /// means the collection evaluates to `true`. The default
    /// behaviour for this method is to return `false`, which is
    /// consistent with those semantics.
    fn empty_collection(&self) -> bool {
        false
    }
    /// Return a value for this operator when applied to a `None`
    /// value wrapping its target type (e.g. for an operator on T, we
    /// are determining the behaviour on Option<T>). This is required
    /// because we automatically implement all operators on Option<T>,
    /// but in rare cases the operator's behaviour isn't well captured
    /// by returning `false` for `None` (e.g for. `isnull`).
    fn null_option(&self) -> bool {
        false
    }
}

/// Something that can make an operator instance from a right-hand side.
///
/// An [`OperatorClass`] can make a particular type of [`Operator`],
/// performing any needed parsing on the right-hand side of the filter
/// expression. The main part of parsing a query is in identifying the
/// [`OperatorClass`] that can handle the requested left hand side; once
/// that has been done, that [`OperatorClass`] will take over via
/// [`instantiate`](OperatorClass::instantiate) and construct the
/// particular [`Operator`] instance to use for that query.
pub trait OperatorClass<T> {
    /// The type of [`Operator`] this object can make.
    type Instance: Operator<T>;
    /// Create a new [`Operator`], parsing the supplied argument string
    /// to initialise whatever representation this operator uses to
    /// match against objects. Since this is a parsing operation, it
    /// can fail.
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError>;
}

/// Test whether an objects to see whether it is included in a given
/// result set.
///
/// Each stanza of a query-string results in one [`Filter`] being
/// constructed. A unique codepath is generated for every combination
/// of field and operator, which ends up resulting in an efficient
/// [`Filter`] for each one.
pub trait Filter<R> {
    /// Produce a true/false response indicating an in/out result for the object.
    fn filter_one(&self, data: &R) -> bool;
    /// Helper method to filter an entire vector by this filter. Note
    /// that there is no virtual function call overhead here.
    fn filter_vec(&self, data: &mut Vec<R>) {
        data.retain(|r| self.filter_one(r))
    }
    /// Helper method to filter an entire vector by this filter. Note
    /// that there is no virtual function call overhead here.
    fn filter_ref_vec(&self, data: &mut Vec<&R>) {
        data.retain(|r| self.filter_one(r))
    }
}

/// An object that can make a [`Filter`].
///
/// An [`OperatorSet`] stores all the possible ways to filter a given type,
/// but each one can be parameterised by a RHS supplied by the user.
/// [`FilterClass`] is an object safe trait that allows us to store
/// these possibilities, and instantiate them into real [`Filter`] objects
/// when an appropriate RHS is provided.
pub trait FilterClass<'a, R> {
    /// Create a new [`Filter`], parsing the `rhs`, which is the argument to
    /// whatever [`Operator`] this [`Filter`] encompassses.
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R> + 'a>, FilterError>;
}

struct FilterImpl<F, O> {
    field: F,
    operator: O,
}

impl<F, O> FilterImpl<F, O> {
    pub fn new(field: F, operator: O) -> Self {
        Self { field, operator }
    }
}

impl<'a, F, O, R> Filter<R> for FilterImpl<F, O>
where
    F: Member<'a, R>,
    <F as Member<'a, R>>::Value: Operable,
    O: Operator<<<F as Member<'a, R>>::Value as Operable>::Base>,
{
    fn filter_one(&self, data: &R) -> bool {
        self.field.apply(&self.operator, data)
    }
}

struct FilterClassImpl<F, O> {
    field: F,
    opclass: O,
}

impl<F, O> FilterClassImpl<F, O> {
    pub fn new(field: F, opclass: O) -> Self {
        Self { field, opclass }
    }
}

impl<'a, F, O, R, T> FilterClass<'a, R> for FilterClassImpl<F, O>
where
    F: Member<'a, R, Value = T> + Clone,
    O: OperatorClass<<T as Operable>::Base>,
    <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
    T: Operable,
{
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R> + 'a>, FilterError> {
        Ok(Box::new(FilterImpl::new(
            self.field.clone(),
            self.opclass.instantiate(rhs)?,
        )))
    }
}

/// Something which can produce a description of how to filter it.
///
/// This is the main derivable trait from this file. A type
/// implementing [`Filterable`] can provide an object, [`Meta`], which
/// describes its supported fields and their operators.
pub trait Filterable<'a>: Sized {
    /// The type which can describe our fields and their operators.
    type Meta: Meta<'a, Self>;
    /// Produce an instance of our [`Meta`] type.
    fn get_meta() -> Self::Meta;
}

#[doc(hidden)]
pub struct QueryProcessor<'a, 'q, R> {
    pub target: &'q str,
    pub rhs: &'q str,
    pub result: Option<Result<Box<dyn Filter<R> + 'a>, FilterError>>,
}

impl<'a, 'q, R> MetaVisitor<'a, R> for QueryProcessor<'a, 'q, R> {
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<'a, R, Value = T> + Clone,
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
        T: Operable,
    {
        if self.target == name {
            let cls = FilterClassImpl::new(field.clone(), defop);
            self.result = Some(cls.instantiate(self.rhs));
        } else if self.target.starts_with(name) {
            let mut m = QueryMemberProcessor {
                target: self.target,
                rhs: self.rhs,
                name,
                result: None,
            };
            field.accept_visitor(&mut m);
            self.result = m.result;
        }
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value = U> + Clone + 'a,
        T: Meta<'a, U>,
    {
        if self.target.starts_with(name) {
            let mut n = NestedMetaVisitor {
                parent: self,
                prefix: name.to_string(),
                nester: NesterImpl {
                    outer_field: field.clone(),
                },
                _marker: Default::default(),
            };

            inner_record.accept_visitor(&mut n);
        }
    }
}

struct QueryMemberProcessor<'a, 'b, 'c, R> {
    target: &'a str,
    rhs: &'a str,
    name: &'b str,
    result: Option<Result<Box<dyn Filter<R> + 'c>, FilterError>>,
}

impl<'a, 'b, 'c, F, R, T> MemberVisitor<'c, F, R, T> for QueryMemberProcessor<'a, 'b, 'c, R>
where
    F: Member<'c, R, Value = T> + Clone,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, field: &F, op: O)
    where
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'c,
    {
        if format!("{}__{}", self.name, name) == self.target {
            let cls = FilterClassImpl::new(field.clone(), op);
            self.result = Some(cls.instantiate(self.rhs));
        }
    }
}

/// A collection of filters for a particular type.
///
/// An [`OperatorSet`] can be created for any type which implements
/// [`Filterable`]. Its constructor supplies a visitor to the type's
/// [`Meta`] type, and constructs the necessary [`FilterClass`] objects.
/// It can be used directly to convert incoming query pairs from
/// Django-style query URLs into appropriate [`Filter`] objects.
pub struct OperatorSet<R> {
    _marker: core::marker::PhantomData<R>,
}

impl<'a, R: Filterable<'a>> Default for OperatorSet<R> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, R: Filterable<'a>> OperatorSet<R> {
    /// Create a new [`OperatorSet`], using the [`Meta`] record
    /// from the target type
    pub fn new() -> Self {
        Self {
            _marker: Default::default(),
        }
    }

    /// Create a new filter from a decomposed URL.
    ///
    /// Note that the decomposition is ambiguous, so
    /// [`create_filter_from_query_pair`](OperatorSet::create_filter_from_query_pair)
    /// is often preferable. The ambiguity arises because there may be
    /// no operator part, and the field part may have arbitrarily many
    /// parts.  Since everything is separated with double underscores
    /// there is no way without additional information to distinguish
    /// an expression with an explicit operator from an expression on
    /// a nested field with an implicit operator.  Here
    /// - `field` is the field specification part of the string,
    ///    including any nested fields.
    /// - `operator` is the operator specification, if any.
    /// - `rhs` is the argument part, after the `=` in the URL
    ///    section.
    pub fn create_filter(
        &self,
        field: &str,
        operator: Option<&str>,
        rhs: &str,
    ) -> Result<Box<dyn Filter<R> + 'a>, FilterError> {
        let lhs = if let Some(op) = operator {
            format!("{}__{}", field, op)
        } else {
            field.to_string()
        };

        self.create_filter_from_query_pair(&lhs, rhs)
    }

    /// Create a new filter from a fragment of query URL.
    ///
    /// The expected input is a `lhs=rhs` pair from a query string,
    /// with `lhs` and `rhs` given separately. The output will either
    /// be an error, or a [`Filter`] which can apply the specified
    /// test to objects of the target type, returning either `true`
    /// (include) or `false` (exclude).
    pub fn create_filter_from_query_pair(
        &self,
        lhs: &str,
        rhs: &str,
    ) -> Result<Box<dyn Filter<R> + 'a>, FilterError> {
        let mut p = QueryProcessor {
            target: lhs,
            rhs,
            result: None,
        };

        R::get_meta().accept_visitor(&mut p);

        p.result
            .ok_or_else(|| FilterError::NoField(lhs.to_string()))?
    }

    /// Create a new filter from a fragment of query URL.
    ///
    /// The expected input is a `lhs=rhs` pair from a query string,
    /// with `lhs` and `rhs` given together in a single string. The
    /// output will either be an error, or a [`Filter`] which can apply
    /// the specified test to objects of the target type, returning
    /// either `true` (include) or `false` (exclude).
    pub fn create_filter_from_query(
        &self,
        expr: &str,
    ) -> Result<Box<dyn Filter<R> + 'a>, FilterError> {
        let parts = expr.splitn(2, '=').collect::<Vec<_>>();
        if parts.len() != 2 {
            Err(FilterError::MissingEquals)
        } else {
            self.create_filter_from_query_pair(parts[0], parts[1])
        }
    }
}

struct NestedMetaVisitor<'p, F, R, P> {
    parent: &'p mut P,
    prefix: String,
    nester: NesterImpl<F>,
    _marker: std::marker::PhantomData<R>,
}

impl<'a, 'p, R, G, P, S> MetaVisitor<'a, S> for NestedMetaVisitor<'p, G, R, P>
where
    G: Field<R, Value = S> + Clone + 'a,
    P: MetaVisitor<'a, R>,
{
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<'a, S, Value = T> + Clone,
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
        T: Operable,
    {
        self.parent.visit_member(
            format!("{}__{}", self.prefix, name).as_str(),
            &self.nester.nest_member(field.clone()),
            defop,
        );
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<S, Value = U> + Clone + 'a,
        T: Meta<'a, U>,
    {
        let name = format!("{}__{}", self.prefix, name);
        let mut n = NestedMetaVisitor {
            parent: self.parent,
            prefix: name,
            nester: NesterImpl {
                outer_field: self.nester.nest_field(field.clone()),
            },
            _marker: Default::default(),
        };
        inner_record.accept_visitor(&mut n);
    }
}

struct NestedMemberVisitor<'a, F, G, R, P> {
    parent: &'a mut P,
    field: &'a NestedField<F, G>,
    _marker: core::marker::PhantomData<R>,
}

impl<'a, 'b, F, G, P, R, S, T> MemberVisitor<'b, G, S, T> for NestedMemberVisitor<'a, F, G, R, P>
where
    P: MemberVisitor<'b, NestedField<F, G>, R, T>,
    F: Field<R, Value = S> + 'b,
    G: Member<'b, S, Value = T>,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &G, op: O)
    where
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'b,
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

#[derive(Clone)]
struct IterableField<F> {
    inner_field: F,
}

impl<F, R, T, I> Field<I> for IterableField<F>
where
    F: Field<R, Value = T>,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
{
    type Value = T;
    fn apply<O: Operator<T>>(&'_ self, op: &O, data: &I) -> bool {
        data.into_iter().any(|x| self.inner_field.apply(op, x))
    }
}

impl<'a, F, R, T, I> Member<'a, I> for IterableField<F>
where
    F: Member<'a, R, Value = T>,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
    T: Operable,
{
    type Value = T;
    fn apply<O: Operator<<T as Operable>::Base>>(&'_ self, op: &O, data: &I) -> bool {
        data.into_iter().any(|x| self.inner_field.apply(op, x))
    }
    fn accept_visitor<V: MemberVisitor<'a, Self, I, <Self as Member<'a, I>>::Value>>(
        &self,
        visitor: &mut V,
    ) {
        let mut n = IterableMemberVisitor {
            parent: visitor,
            field: self,
            _marker: Default::default(),
        };
        self.inner_field.accept_visitor(&mut n);
    }
}

struct IterableMemberVisitor<'a, F, I, P> {
    parent: &'a mut P,
    field: &'a IterableField<F>,
    _marker: core::marker::PhantomData<I>,
}

impl<'a, 'b, F, P, R, T, I> MemberVisitor<'b, F, R, T> for IterableMemberVisitor<'a, F, I, P>
where
    P: MemberVisitor<'b, IterableField<F>, I, T>,
    F: Member<'b, R, Value = T>,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, op: O)
    where
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'b,
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

#[doc(hidden)]
pub struct IterableMeta;

impl<'a, R, I> Meta<'a, I> for IterableMeta
where
    R: Filterable<'a>,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
{
    fn accept_visitor<V: MetaVisitor<'a, I>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut n = IterableMetaVisitor {
            parent: visitor,
            _marker: Default::default(),
        };
        R::get_meta().accept_visitor(&mut n);
    }
}

struct IterableMetaVisitor<'p, P, I> {
    parent: &'p mut P,
    _marker: core::marker::PhantomData<I>,
}

impl<'a, 'p, P, R, I> MetaVisitor<'a, R> for IterableMetaVisitor<'p, P, I>
where
    P: MetaVisitor<'a, I>,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
{
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<'a, R, Value = T> + Clone,
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
        T: Operable,
    {
        self.parent.visit_member(
            name,
            &IterableField {
                inner_field: field.clone(),
            },
            defop,
        );
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value = U> + Clone + 'a,
        T: Meta<'a, U>,
    {
        self.parent.visit_record(
            name,
            &IterableField {
                inner_field: field.clone(),
            },
            inner_record,
        )
    }
}

struct PrintingVisitor {
    prefix: Option<String>,
}

impl<'a, R> MetaVisitor<'a, R> for PrintingVisitor {
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, _defop: O)
    where
        F: Member<'a, R, Value = T> + Clone,
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
        T: Operable,
    {
        let name = if let Some(ref prefix) = self.prefix {
            format!("{}__{}", prefix, name)
        } else {
            name.to_string()
        };
        println!("{}", name);
        let mut pv = PrintingVisitor { prefix: Some(name) };
        field.accept_visitor(&mut pv);
    }
    fn visit_record<F, T, U>(&mut self, name: &str, _field: &F, inner_record: &T)
    where
        F: Field<R, Value = U> + Clone + 'a,
        T: Meta<'a, U>,
    {
        let new_prefix = if let Some(ref prefix) = self.prefix {
            format!("{}__{}", prefix, name)
        } else {
            name.to_string()
        };
        let mut pv = PrintingVisitor {
            prefix: Some(new_prefix),
        };
        inner_record.accept_visitor(&mut pv);
    }
}

impl<'a, F, R, T> MemberVisitor<'a, F, R, T> for PrintingVisitor
where
    F: Member<'a, R, Value = T> + Clone,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, _op: O)
    where
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
    {
        // unwrap is safe: there are no bare operators
        println!("{}__{}", self.prefix.as_ref().unwrap(), name);
    }
}

/// Debug print of a [`Filterable`] type showing the supported
/// queries.
///
/// Example:
/// ```rust
/// use django_query::filtering::{print_filters, Filterable};
///
/// #[derive(Filterable)]
/// struct Foo {
///   #[django(op(lt,gt,gte,lte))]
///   a: i32
/// }
///
/// print_filters::<Foo>();
/// ```
/// produces
/// ```text
/// a
/// a__gt
/// a__gte
/// a__lt
/// a__lte
/// ```
pub fn print_filters<'a, Q: Filterable<'a>>() {
    let mut pv = PrintingVisitor { prefix: None };
    Q::get_meta().accept_visitor(&mut pv)
}

impl<'a, T> Filterable<'a> for Vec<T>
where
    T: Filterable<'a>,
{
    type Meta = IterableMeta;
    fn get_meta() -> Self::Meta {
        IterableMeta
    }
}

impl<'a, T> Filterable<'a> for Option<T>
where
    T: Filterable<'a>,
{
    type Meta = IterableMeta;
    fn get_meta() -> Self::Meta {
        IterableMeta
    }
}

impl<'a, T> Filterable<'a> for Arc<T>
where
    T: Filterable<'a>,
{
    type Meta = ArcMeta;
    fn get_meta() -> Self::Meta {
        ArcMeta
    }
}

#[doc(hidden)]
pub struct ArcMeta;

impl<'a, R> Meta<'a, Arc<R>> for ArcMeta
where
    R: Filterable<'a>,
{
    fn accept_visitor<V: MetaVisitor<'a, Arc<R>>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut n = ArcMetaVisitor { parent: visitor };
        R::get_meta().accept_visitor(&mut n);
    }
}

struct ArcMetaVisitor<'p, P> {
    parent: &'p mut P,
}

impl<'a, 'p, P, R> MetaVisitor<'a, R> for ArcMetaVisitor<'p, P>
where
    P: MetaVisitor<'a, Arc<R>>,
{
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<'a, R, Value = T> + Clone,
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'a,
        T: Operable,
    {
        self.parent.visit_member(
            name,
            &WrapperField {
                inner_field: field.clone(),
            },
            defop,
        );
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value = U> + Clone + 'a,
        T: Meta<'a, U>,
    {
        self.parent.visit_record(
            name,
            &WrapperField {
                inner_field: field.clone(),
            },
            inner_record,
        )
    }
}

#[derive(Clone)]
struct WrapperField<F> {
    inner_field: F,
}

impl<F, R, T, U> Field<U> for WrapperField<F>
where
    F: Field<R, Value = T>,
    U: Deref<Target = R>,
{
    type Value = T;
    fn apply<O: Operator<T>>(&'_ self, op: &O, data: &U) -> bool {
        self.inner_field.apply(op, &*data)
    }
}

impl<'a, F, R, T, U> Member<'a, U> for WrapperField<F>
where
    F: Member<'a, R, Value = T>,
    U: Deref<Target = R>,
    T: Operable,
{
    type Value = T;
    fn apply<O: Operator<<T as Operable>::Base>>(&'_ self, op: &O, data: &U) -> bool {
        self.inner_field.apply(op, &*data)
    }
    fn accept_visitor<V: MemberVisitor<'a, Self, U, <Self as Member<'a, U>>::Value>>(
        &self,
        visitor: &mut V,
    ) {
        let mut n = WrapperMemberVisitor {
            parent: visitor,
            field: self,
            _marker: Default::default(),
        };
        self.inner_field.accept_visitor(&mut n);
    }
}

struct WrapperMemberVisitor<'a, F, P, U> {
    parent: &'a mut P,
    field: &'a WrapperField<F>,
    _marker: core::marker::PhantomData<U>,
}

impl<'a, 'b, F, P, R, T, U> MemberVisitor<'b, F, R, T> for WrapperMemberVisitor<'a, F, P, U>
where
    P: MemberVisitor<'b, WrapperField<F>, U, T>,
    F: Member<'b, R, Value = T>,
    U: Deref<Target = R>,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, op: O)
    where
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'b,
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

// State support

/// Something that can describe how to filter itself if a context type
/// is provided.
///
/// Something implementing this trait can provide a [`Meta`] which
/// describes its supported fields and their operators, but only when
/// a context object is provided.  Note that the provided context
/// object may be stored within the [`Meta`] type, which is the origin
/// of the lifetime parameter.
///
#[cfg_attr(
    feature = "persian-rug",
    doc = r##"
This can be derived via the
[`FilterableWithPersianRug`](FilterableWithPersianRug)
derive macro for the case of a [`persian-rug`](::persian_rug)
type.
"##
)]
pub trait FilterableWithContext<'a, A: 'a>: Sized {
    /// `Meta` is the type which can describe our fields and their operators.
    type Meta: Meta<'a, Self>;
    /// `get_meta` produces an instance of our `Meta` type.
    ///
    /// Here `access` is some context object that we need in order to
    /// provide filters for our type.
    fn get_meta(access: A) -> Self::Meta;
}

/// A collection of filters for a type that requires a context object.
///
/// An [`OperatorSetWithContext`] can be created for any type which
/// implements [`FilterableWithContext`]. Its constructor supplies a
/// visitor to the type's `Meta` type, and constructs the necessary
/// [`FilterClass`] objects.  It can be used directly to convert
/// incoming query pairs from Django-style query URLs into appropriate
/// [`Filter`] objects.
///
/// Note that the context object provided on construction may be
/// stored within the resulting object, and may also be cloned into
/// the [`Meta`] for other types.
pub struct OperatorSetWithContext<R, A> {
    _marker: core::marker::PhantomData<R>,
    access: A,
}

impl<'a, A: Clone + 'a, R: FilterableWithContext<'a, A>> OperatorSetWithContext<R, A> {
    /// Create a new [`OperatorSetWithContext`], using the [`Meta`] record
    /// from the target type, which will be initialized from the provided
    /// context object `access`.
    pub fn new(access: A) -> Self {
        Self {
            _marker: Default::default(),
            access,
        }
    }

    /// Create a new filter from a decomposed URL. Note that the
    /// decomposition is ambiguous, so
    /// [`create_filter_from_query_pair`](OperatorSetWithContext::create_filter_from_query_pair)
    /// is often preferable. The ambiguity arises because there may be
    /// no operator part, and the field part may have arbitrarily many
    /// parts.  Since everything is separated with double underscores
    /// there is no way without additional information to distinguish
    /// an expression with an explicit operator from an expression on
    /// a nested field with an implicit operator.  Here - `field` is
    /// the field specification part of the string, including any
    /// nested fields.  - `operator` is the operator specification, if
    /// any.  - `rhs` is the argument part, after the `=` in the URL
    /// section.
    pub fn create_filter(
        &self,
        field: &str,
        operator: Option<&str>,
        rhs: &str,
    ) -> Result<Box<dyn Filter<R> + 'a>, FilterError> {
        let lhs = if let Some(op) = operator {
            format!("{}__{}", field, op)
        } else {
            field.to_string()
        };

        self.create_filter_from_query_pair(&lhs, rhs)
    }

    /// Create a new filter from a fragment of query URL. The expected
    /// input is a `lhs=rhs` pair from a query string, with `lhs` and
    /// `rhs` given separately. The output will either be an error, or
    /// a [`Filter`] which can apply the specified test to objects of
    /// the target type, returning either `true` (include) or `false`
    /// (exclude).
    pub fn create_filter_from_query_pair(
        &self,
        lhs: &str,
        rhs: &str,
    ) -> Result<Box<dyn Filter<R> + 'a>, FilterError> {
        let mut p = QueryProcessor {
            target: lhs,
            rhs,
            result: None,
        };

        R::get_meta(self.access.clone()).accept_visitor(&mut p);

        p.result
            .ok_or_else(|| FilterError::NoField(lhs.to_string()))?
    }

    /// Create a new filter from a fragment of query URL. The expected
    /// input is a `lhs=rhs` pair from a query string, with `lhs` and
    /// `rhs` given together in a single string. The output will
    /// either be an error, or a [`Filter`] which can apply the
    /// specified test to objects of the target type, returning
    /// either `true` (include) or `false` (exclude).
    pub fn create_filter_from_query(
        &self,
        expr: &str,
    ) -> Result<Box<dyn Filter<R> + 'a>, FilterError> {
        let parts = expr.splitn(2, '=').collect::<Vec<_>>();
        if parts.len() != 2 {
            Err(FilterError::MissingEquals)
        } else {
            self.create_filter_from_query_pair(parts[0], parts[1])
        }
    }
}

#[doc(hidden)]
pub struct IterableMetaWithContext<A> {
    access: A,
}

impl<'a, A, R, I> Meta<'a, I> for IterableMetaWithContext<A>
where
    A: 'a + Clone,
    R: FilterableWithContext<'a, A>,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
{
    fn accept_visitor<V: MetaVisitor<'a, I>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut n = IterableMetaVisitor {
            parent: visitor,
            _marker: Default::default(),
        };
        R::get_meta(self.access.clone()).accept_visitor(&mut n);
    }
}

impl<'a, A, T> FilterableWithContext<'a, A> for Option<T>
where
    T: FilterableWithContext<'a, A>,
    A: 'a + Clone,
{
    type Meta = IterableMetaWithContext<A>;
    fn get_meta(access: A) -> Self::Meta {
        IterableMetaWithContext { access }
    }
}

impl<'a, A, T> FilterableWithContext<'a, A> for Vec<T>
where
    T: FilterableWithContext<'a, A>,
    A: 'a + Clone,
{
    type Meta = IterableMetaWithContext<A>;
    fn get_meta(access: A) -> Self::Meta {
        IterableMetaWithContext { access }
    }
}

pub use django_query_derive::Filterable;

#[cfg(feature = "persian-rug")]
#[cfg_attr(docsrs, doc(cfg(feature = "persian-rug")))]
pub use crate::persian_rug::FilterableWithPersianRug;
