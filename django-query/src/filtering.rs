//! Create filters for Rust objects from query URLs.
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
//! different table, then it's possible to filter within foo. Suppose
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
//! The main trait in this module is [Filterable] which has an
//! associated derive macro. Deriving [Filterable] means it's possible
//! to construct a [OperatorSet] for a type. This can be given query
//! strings, and will convert them into boxed [Filter] objects. The
//! virtual function call overhead is right at the top here, in the
//! call to the filter object, so where possible filtering entire
//! vectors at a time is preferable to reduce overheads (since each
//! call into the boxed [Filter] incurs overhead, but only one such
//! call is needed to filter an entire collection.)
//!
//! Example
//! ```rust
//! use django_query::{Filterable, OperatorSet};
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
//! Note that self-including types - types that have members of their
//! own type, at some level of indirection - cannot be handled by this
//! crate because of the commitment to monomorphism. It must always be
//! possible to write out all the valid left hand sides that could
//! occur in a filter, and this will in fact be done automatically
//! when a [OperatorSet] is constructed.

use core::ops::Deref;

use std::collections::BTreeMap;
use std::fmt::Debug;
use std::sync::Arc;

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

/// A type that Django operators can work on.
///
/// Django queries have operators for simple values, like strings and
/// numbers, but this masks some complexity when nullable values and
/// collections are considered. An `Operable` type is one that an
/// operator on simple values can be applied to.
///
/// All simple types that occur naturally in Django queries can be
/// made `Operable`, but so can, for example, [Vec] and [Option], when
/// their wrapped type is itself `Operable`. It's up to an `Operable`
/// implementation to decide how to take an operator on its base type
/// and apply it to its contents, for example by iterating over
/// whatever contents it has.
///
/// The implementations for [Vec] and [Option] match the default
/// behaviour in Django.
pub trait Operable {
    /// The simple underlying type.
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

/// A wrapper for one field of another type.
///
/// A `Field` allows us to apply an [Operator] to one field of a
/// structure. You will normally generate implementations of Field
/// using the [Filterable] derive macro for your type. The key use of
/// Field is in traversing nested structures: whereas a [Member]
/// represents applying an operator to an [Operable] value, the value
/// of a Field can be arbitrary, and is often a structured type.
pub trait Field<R>: Clone {
    /// The type an operator on this field must accept.
    type Value;
    /// Apply `op` to a particular field of `data`, and return the
    /// result.
    fn apply<O: Operator<Self::Value>>(&self, op: &O, data: &R) -> bool;
}

/// A wrapper for one field of another type, which has [Operable] type.
///
/// A `Member` is an exposed field of a structure. You will normally
/// generate implementations of `Member` using the [Filterable] derive
/// macro for your type. Whereas a [Field] is just about access for
/// mechanical reasons (nesting), a `Member` is visible in queries,
/// and can expose a list of supported query operators to a
/// [MemberVisitor].
pub trait Member<R>: Clone {
    /// The type of the member's data, which must be [Operable] so
    /// that Django filtering queries (which are defined on simple
    /// types) can be made against it.
    type Value: Operable;
    /// Apply an operator to the given field of `data`, returning the
    /// result.
    fn apply<O: Operator<<Self::Value as Operable>::Base>>(&self, op: &O, data: &R) -> bool;
    /// `visitor` will be called with each of the supported operator
    /// classes for this member. This is used to build up a list
    /// of supported queries in a [OperatorSet].
    fn accept_visitor<V: MemberVisitor<Self, R, Self::Value>>(&self, visitor: &mut V);
}

/// Something that can receive callbacks about the operators a member
/// supports.
///
/// A `MemberVisitor` can be passed to [Member::accept_visitor] to
/// receive a callback for each of the operator classes a given
/// [Member] supports.
pub trait MemberVisitor<F, R, T>
where
    F: Member<R, Value = T> + Clone,
{
    /// This is called by [Member] for each of the operator classes it
    /// supports. Here:
    /// - `name` is the name of the operator in queries (e.g. "in")
    /// - `f` is the calling [Member] itself.
    /// - `op` is an [OperatorClass] with a target type matching the
    ///   `Member`.
    fn visit_operator<O>(&mut self, name: &str, f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable;
}

/// A type holding metadata about another type.
///
/// A `Meta` holds a representation of an exposed structure, similar
/// to [Member] for fields, but now for an entire structured
/// type. When a type is [Filterable] it can produce a `Meta` for
/// itself, to enable discovery of its available members and their
/// supported operators.
pub trait Meta<R> {
    /// `visitor` will be called with each exposed member of the type.
    fn accept_visitor<V: MetaVisitor<R>>(&self, visitor: &mut V)
    where
        Self: Sized;
}

/// Something that can receive callbacks about the members and fields
/// in a [Meta].
///
/// A `MetaVisitor` can be passed to [Meta::accept_visitor] to
/// receive a callback for each of the exposed members of a type.
pub trait MetaVisitor<R> {
    /// This should be called by [Meta] for each exposed structure
    /// field which is [Operable]. Here:
    /// - `name` is the name of the field, as exposed.
    /// - `member` is the [Member] for that field.
    /// - `defop` is the default [OperatorClass] for that field, which
    ///    should be applied when no operator is specified (i.e. in
    ///    the `"record__field"` case as opposed to the
    ///    `"record__field__op"` case)
    fn visit_member<F, O, T>(&mut self, name: &str, member: &F, defop: O)
    where
        F: Member<R, Value = T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable;

    /// This should be called by [Meta] for each exposed structure field
    /// which can be traversed, that is where the type of the field is itself
    /// a structured type, for example when modelling foreign keys. Here:
    /// - `name` is the name of the field, as exposed.
    /// - `field` is the [Field] for that field (note the difference:
    ///    there are no exposed operators for a Field, and operators
    ///    are applied to the record type, since there could be no one
    ///    [Operable::Base] type.
    /// - `inner_record` is the [Meta] for the field.
    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value = U> + Clone + 'static,
        T: Meta<U> + 'static,
        U: 'static;
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

impl<'a, T, O, F, S> Operator<S> for MemberNestingOperator<'a, O, S, F>
where
    O: Operator<<T as Operable>::Base>,
    F: Member<S, Value = T>,
    T: Operable,
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
    S: 'static,
    T: Operable,
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

impl<F, G, R, S, T> Member<R> for NestedField<F, G>
where
    F: Field<R, Value = S> + 'static,
    G: Member<S, Value = T>,
    S: 'static,
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

    fn accept_visitor<V: MemberVisitor<Self, R, <Self as Member<R>>::Value>>(
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

trait Nester<R, U, F>
where
    F: Field<U, Value = R>,
{
    fn nest<G, T>(&self, nested_field: G) -> NestedField<F, G>
    where
        G: Member<R, Value = T> + 'static,
        T: Operable,
        R: 'static;
}

struct NesterImpl<F> {
    outer_field: F,
}

impl<R, U, F> Nester<R, U, F> for NesterImpl<F>
where
    F: Field<U, Value = R> + Clone,
{
    fn nest<G, T>(&self, inner_field: G) -> NestedField<F, G>
    where
        G: Member<R, Value = T> + 'static,
        T: Operable,
        R: 'static,
    {
        NestedField {
            outer_field: self.outer_field.clone(),
            inner_field,
        }
    }
}

/// Take a single simple value and produce a true/false result.
///
/// Operators are generally applied by [Operable] types, so the
/// operators themselves can be naive with respect to things like
/// [Option] and collections. For operators that need to know about
/// special cases, there are the inelegant get-out functions
/// [empty_collection()](Operator::empty_collection) and
/// [null_option()](Operator::null_option) which can be
/// implemented. Both return `false` by default.
///
/// The standard Django operators are in the
/// [operators](crate::operators) module. Examples are
/// [`eq`](crate::operators::EqImpl),
/// [`in`](crate::operators::InImpl),
/// [`contains`](crate::operators::ContainsImpl) and so forth.
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
    /// by returning `false` for `None` (e.g. `isnull).
    fn null_option(&self) -> bool {
        false
    }
}

/// Something that can make an operator instance from a right-hand side.
///
/// An `OperatorClass` can make a particular type of [Operator],
/// performing any needed parsing on the right-hand side of the filter
/// expression. The main part of parsing a query is in identifying the
/// `OperatorClass` that can handle the requested left hand side; once
/// that has been done, that object will take over via
/// [instantiate()](OperatorClass::instantiate) and construct the
/// particular [Operator] instance to use for that query.
pub trait OperatorClass<T> {
    /// The type of [Operator] this object can make.
    type Instance: Operator<T>;
    /// Create a new [Operator], parsing the supplied argument string
    /// to initialise whatever representation this operator uses to
    /// match against objects. Since this is a parsing operation, it
    /// can fail.
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError>;
}

/// An object representing a test for objects to see whether they are
/// included in a given result set.
///
/// Each stanza of a query results in one `Filter` being
/// constructed. There is no dynamic dispatch below this point in
/// general, a unique codepath is generated for every combination of
/// field and operator, which ends up resulting in a filter for each
/// one of these.
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

/// An object that can make a [Filter].
///
/// To be useful, we need to be able to store objects that can make
/// different filters for the same target type.  That means we need to
/// return a `Box<dyn Filter<R>>` because we can't make the actual
/// type we return observable (if we did, we could no longer store
/// different implementors of this trait together).
pub trait FilterClass<R> {
    /// Create a new `Filter`, parsing the `rhs`, which are arguments to
    /// whatever `Operator` this `Filter` encompassses.
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R>>, FilterError>;
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

impl<F, O, T, R> Filter<R> for FilterImpl<F, O>
where
    F: Member<R, Value = T>,
    O: Operator<<T as Operable>::Base>,
    T: Operable,
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

impl<F, O, R, T> FilterClass<R> for FilterClassImpl<F, O>
where
    F: Member<R, Value = T> + Clone + 'static,
    O: OperatorClass<<T as Operable>::Base>,
    <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
    T: Operable,
{
    fn instantiate(&self, rhs: &str) -> Result<Box<dyn Filter<R>>, FilterError> {
        Ok(Box::new(FilterImpl::new(
            self.field.clone(),
            self.opclass.instantiate(rhs)?,
        )))
    }
}

/// Something that can produce a [Meta] for itself.
///
/// The derivable trait from this file. A type implementing
/// `Filterable` can provide an object which describes its supported
/// fields and their operators.
pub trait Filterable: Sized {
    /// `Meta` is the type which can describe our fields and their operators.
    type Meta: Meta<Self>;
    /// `get_meta` produces an instance of our `Meta` type.
    fn get_meta() -> Self::Meta;
}

struct FilterableMember<R> {
    default: Box<dyn FilterClass<R>>,
    operators: BTreeMap<String, Box<dyn FilterClass<R>>>,
}

impl<R> FilterableMember<R> {
    pub fn new<F, O, T>(f: &F, defop: O) -> Self
    where
        F: Member<R, Value = T> + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable,
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

impl<F, R, T> MemberVisitor<F, R, T> for FilterableMember<R>
where
    F: Member<R, Value = T> + Clone,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
    {
        self.operators.insert(
            name.to_string(),
            Box::new(FilterClassImpl::new(f.clone(), op)),
        );
    }
}

/// A collection of filters for a particular type.
///
/// A [OperatorSet] can be created for any type which implements
/// [Filterable]. Its constructor supplies a visitor to the type's
/// `Meta` type, and constructs the necessary [FilterClass] objects.
/// It can be used directly to convert incoming query pairs from
/// Django-style query URLs into appropriate [Filter] objects.
pub struct OperatorSet<R> {
    // map from field names to supported operators
    fields: BTreeMap<String, FilterableMember<R>>,
}

impl<R: Filterable> Default for OperatorSet<R> {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: Filterable> OperatorSet<R> {
    /// Create a new `OperatorSet`, by obtaining the `Meta` record
    /// from the target type, and visiting its fields and their
    /// operators.
    pub fn new() -> Self {
        let mut res = Self {
            fields: BTreeMap::new(),
        };
        R::get_meta().accept_visitor(&mut res);
        res
    }

    /// Create a new filter from a decomposed URL. Note that the
    /// decomposition is ambiguous, so `create_filter_from_query_pair`
    /// is often preferable. The ambiguity arises because there may be
    /// no operator part, and the field part may have arbitrarily many
    /// parts.  Since everything is separated with double underscores
    /// there is no way without additional information to distinguish
    /// an expression with an explicit operator from an expression on
    /// a nested field with an implicit operator.
    /// Here
    /// - `field` is the field specification part of the string,
    ///    including any nested fields.
    /// - `operator` is the operator specification, if any.
    /// - `rhs` is the argument part, after the `=` in the URL section.
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

    /// Create a new filter from a fragment of query URL. The expected
    /// input is a `lhs=rhs` pair from a query string, with `lhs` and
    /// `rhs` given separately. The output will either be an error, or
    /// a `Filter` which can apply the specified to test to objects of
    /// the target type, returning either `true` (include) or `false`
    /// (exclude).
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

    /// Create a new filter from a fragment of query URL. The expected
    /// input is a `lhs=rhs` pair from a query string, with `lhs` and
    /// `rhs` given together in a single string. The output will
    /// either be an error, or a `Filter` which can apply the
    /// specified to test to objects of the target type, returning
    /// either `true` (include) or `false` (exclude).
    pub fn create_filter_from_query(&self, expr: &str) -> Result<Box<dyn Filter<R>>, FilterError> {
        let parts = expr.splitn(2, '=').collect::<Vec<_>>();
        if parts.len() != 2 {
            Err(FilterError::MissingEquals)
        } else {
            self.create_filter_from_query_pair(parts[0], parts[1])
        }
    }
}

impl<R> MetaVisitor<R> for OperatorSet<R> {
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<R, Value = T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable,
    {
        self.fields
            .insert(name.to_string(), FilterableMember::new(field, defop));
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<R, Value = U> + Clone + 'static,
        T: Meta<U> + 'static,
        U: 'static,
    {
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

struct NestedMetaVisitor<'a, F, R, P> {
    parent: &'a mut P,
    prefix: String,
    nester: NesterImpl<F>,
    _marker: std::marker::PhantomData<R>,
}

impl<'a, R, G, P, S> MetaVisitor<S> for NestedMetaVisitor<'a, G, R, P>
where
    G: Field<R, Value = S> + Clone + 'static,
    P: MetaVisitor<R>,
    S: 'static,
{
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<S, Value = T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
        T: Operable,
    {
        self.parent.visit_member(
            format!("{}__{}", self.prefix, name).as_str(),
            &self.nester.nest(field.clone()),
            defop,
        );
    }

    fn visit_record<F, T, U>(&mut self, name: &str, field: &F, inner_record: &T)
    where
        F: Field<S, Value = U> + Clone + 'static,
        T: Meta<U> + 'static,
        U: 'static,
    {
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

struct NestedMemberVisitor<'a, F, G, R, P> {
    parent: &'a mut P,
    field: &'a NestedField<F, G>,
    _marker: core::marker::PhantomData<R>,
}

impl<'a, F, G, P, R, S, T> MemberVisitor<G, S, T> for NestedMemberVisitor<'a, F, G, R, P>
where
    P: MemberVisitor<NestedField<F, G>, R, T>,
    F: Field<R, Value = S> + 'static,
    G: Member<S, Value = T>,
    S: 'static,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &G, op: O)
    where
        G: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
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
    R: 'static,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
{
    type Value = T;
    fn apply<O: Operator<T>>(&'_ self, op: &O, data: &I) -> bool {
        data.into_iter().any(|x| self.inner_field.apply(op, x))
    }
}

impl<F, R, T, I> Member<I> for IterableField<F>
where
    F: Member<R, Value = T> + 'static,
    R: 'static,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
    T: Operable,
{
    type Value = T;
    fn apply<O: Operator<<T as Operable>::Base>>(&'_ self, op: &O, data: &I) -> bool {
        data.into_iter().any(|x| self.inner_field.apply(op, x))
    }
    fn accept_visitor<V: MemberVisitor<Self, I, <Self as Member<I>>::Value>>(
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

impl<'a, F, P, R, T, I> MemberVisitor<F, R, T> for IterableMemberVisitor<'a, F, I, P>
where
    P: MemberVisitor<IterableField<F>, I, T>,
    F: Member<R, Value = T> + 'static,
    R: 'static,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

#[doc(hidden)]
pub struct IterableMeta;

impl<R, I> Meta<I> for IterableMeta
where
    R: Filterable + 'static,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
{
    fn accept_visitor<V: MetaVisitor<I>>(&self, visitor: &mut V)
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

struct IterableMetaVisitor<'a, P, I> {
    parent: &'a mut P,
    _marker: core::marker::PhantomData<I>,
}

impl<'a, P, R, I> MetaVisitor<R> for IterableMetaVisitor<'a, P, I>
where
    P: MetaVisitor<I>,
    R: 'static,
    for<'i> &'i I: IntoIterator<Item = &'i R>,
{
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<R, Value = T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
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
        F: Field<R, Value = U> + Clone + 'static,
        T: Meta<U> + 'static,
        U: 'static,
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

impl<R> MetaVisitor<R> for PrintingVisitor {
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, _defop: O)
    where
        F: Member<R, Value = T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
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
        F: Field<R, Value = U> + Clone + 'static,
        T: Meta<U> + 'static,
        U: 'static,
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

impl<F, R, T> MemberVisitor<F, R, T> for PrintingVisitor
where
    F: Member<R, Value = T> + Clone,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, _op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
    {
        // unwrap is safe: there are no bare operators
        println!("{}__{}", self.prefix.as_ref().unwrap(), name);
    }
}

/// Debug print of a [Filterable] type `Q`, showing the supported
/// queries.
///
/// Example:
/// ```rust
/// use django_query::{filtering::print_filters, Filterable};
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
pub fn print_filters<Q: Filterable>() {
    let mut pv = PrintingVisitor { prefix: None };
    Q::get_meta().accept_visitor(&mut pv)
}

impl<T> Filterable for Vec<T>
where
    T: Filterable + 'static,
{
    type Meta = IterableMeta;
    fn get_meta() -> Self::Meta {
        IterableMeta
    }
}

impl<T> Filterable for Option<T>
where
    T: Filterable + 'static,
{
    type Meta = IterableMeta;
    fn get_meta() -> Self::Meta {
        IterableMeta
    }
}

impl<T> Filterable for Arc<T>
where
    T: Filterable + 'static,
{
    type Meta = ArcMeta;
    fn get_meta() -> Self::Meta {
        ArcMeta
    }
}

#[doc(hidden)]
pub struct ArcMeta;

impl<R> Meta<Arc<R>> for ArcMeta
where
    R: Filterable + 'static,
{
    fn accept_visitor<V: MetaVisitor<Arc<R>>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut n = ArcMetaVisitor { parent: visitor };
        R::get_meta().accept_visitor(&mut n);
    }
}

struct ArcMetaVisitor<'a, P> {
    parent: &'a mut P,
}

impl<'a, P, R> MetaVisitor<R> for ArcMetaVisitor<'a, P>
where
    P: MetaVisitor<Arc<R>>,
    R: 'static,
{
    fn visit_member<F, O, T>(&mut self, name: &str, field: &F, defop: O)
    where
        F: Member<R, Value = T> + Clone + 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
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
        F: Field<R, Value = U> + Clone + 'static,
        T: Meta<U> + 'static,
        U: 'static,
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
    R: 'static,
    U: Deref<Target = R>,
{
    type Value = T;
    fn apply<O: Operator<T>>(&'_ self, op: &O, data: &U) -> bool {
        self.inner_field.apply(op, &*data)
    }
}

impl<F, R, T, U> Member<U> for WrapperField<F>
where
    F: Member<R, Value = T> + 'static,
    R: 'static,
    U: Deref<Target = R>,
    T: Operable,
{
    type Value = T;
    fn apply<O: Operator<<T as Operable>::Base>>(&'_ self, op: &O, data: &U) -> bool {
        self.inner_field.apply(op, &*data)
    }
    fn accept_visitor<V: MemberVisitor<Self, U, <Self as Member<U>>::Value>>(
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

impl<'a, F, P, R, T, U> MemberVisitor<F, R, T> for WrapperMemberVisitor<'a, F, P, U>
where
    P: MemberVisitor<WrapperField<F>, U, T>,
    F: Member<R, Value = T> + 'static,
    R: 'static,
    U: Deref<Target = R>,
    T: Operable,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, op: O)
    where
        F: 'static,
        O: OperatorClass<<T as Operable>::Base> + 'static,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'static,
    {
        self.parent.visit_operator(name, self.field, op);
    }
}
