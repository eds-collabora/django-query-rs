use crate::filtering::{FilterableWithContext, Meta, MetaVisitor};

use crate::filtering::{Field, Member, MemberVisitor, Operable, Operator, OperatorClass};

use persian_rug::{Accessor, Contextual, Proxy};

#[persian_rug::constraints(context = C, access(T))]
impl<'a, A, C, T> FilterableWithContext<'a, A> for Proxy<T>
where
    T: FilterableWithContext<'a, A> + Contextual<Context = C>,
    A: Accessor<Context = C> + 'a,
{
    type Meta = ProxyMeta<A>;
    fn get_meta(access: A) -> Self::Meta {
        ProxyMeta { access }
    }
}

pub struct ProxyMeta<A> {
    access: A,
}

#[persian_rug::constraints(context = C, access(T))]
impl<'f, A, C, T> Meta<'f, Proxy<T>> for ProxyMeta<A>
where
    T: FilterableWithContext<'f, A>,
    A: Accessor<Context = C> + 'f,
    T: Contextual<Context = C>,
{
    fn accept_visitor<V: MetaVisitor<'f, Proxy<T>>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut n = ProxyMetaVisitor {
            parent: visitor,
            access: self.access.clone(),
        };
        T::get_meta(self.access.clone()).accept_visitor(&mut n);
    }
}

struct ProxyMetaVisitor<'p, P, A> {
    parent: &'p mut P,
    access: A,
}

#[persian_rug::constraints(context = C, access(R))]
impl<'a, 'p, A, C, P, R> MetaVisitor<'a, R> for ProxyMetaVisitor<'p, P, A>
where
    P: MetaVisitor<'a, Proxy<R>>,
    A: Accessor<Context = C> + 'a,
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
            &ProxyField {
                inner_field: field.clone(),
                access: self.access.clone(),
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
            &ProxyField {
                inner_field: field.clone(),
                access: self.access.clone(),
            },
            inner_record,
        )
    }
}

#[derive(Clone)]
struct ProxyField<F, A> {
    inner_field: F,
    access: A,
}

#[persian_rug::constraints(context = C, access(U))]
impl<A, C, F, T, U> Field<Proxy<U>> for ProxyField<F, A>
where
    F: Field<U, Value = T>,
    A: Accessor<Context = C>,
{
    type Value = T;
    fn apply<O: Operator<T>>(&'_ self, op: &O, data: &Proxy<U>) -> bool {
        self.inner_field.apply(op, self.access.get(data))
    }
}

#[persian_rug::constraints(context = C, access(U))]
impl<'a, A, C, F, T, U> Member<'a, Proxy<U>> for ProxyField<F, A>
where
    F: Member<'a, U, Value = T>,
    T: Operable,
    A: Accessor<Context = C> + 'a,
{
    type Value = T;
    fn apply<O: Operator<<T as Operable>::Base>>(&'_ self, op: &O, data: &Proxy<U>) -> bool {
        self.inner_field.apply(op, self.access.get(data))
    }
    fn accept_visitor<V: MemberVisitor<'a, Self, Proxy<U>, T>>(&self, visitor: &mut V) {
        let mut n = ProxyMemberVisitor {
            parent: visitor,
            field: self,
            _marker: Default::default(),
        };
        self.inner_field.accept_visitor(&mut n);
    }
}

struct ProxyMemberVisitor<'a, A, F, P, U> {
    parent: &'a mut P,
    field: &'a ProxyField<F, A>,
    _marker: core::marker::PhantomData<U>,
}

#[persian_rug::constraints(context = C, access(U))]
impl<'a, 'b, A, C, F, P, T, U> MemberVisitor<'b, F, U, T> for ProxyMemberVisitor<'a, A, F, P, U>
where
    P: MemberVisitor<'b, ProxyField<F, A>, Proxy<U>, T>,
    F: Member<'b, U, Value = T>,
    T: Operable,
    A: Accessor<Context = C> + 'b,
{
    fn visit_operator<O>(&mut self, name: &str, _f: &F, op: O)
    where
        O: OperatorClass<<T as Operable>::Base>,
        <O as OperatorClass<<T as Operable>::Base>>::Instance: 'b,
    {
        self.parent.visit_operator(name, self.field, op);
    }
}

pub use django_query_derive::FilterableWithPersianRug;
