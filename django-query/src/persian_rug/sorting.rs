use crate::sorting::{Field, SortVisitor, Sorter, SorterTypedClass};
use crate::sorting::{Meta, SortableWithContext};

use core::cmp::Ordering;

use persian_rug::{Accessor, Proxy};

#[persian_rug::constraints(context=C, access(T))]
impl<'s, A, T, C> SortableWithContext<'s, A> for Proxy<T>
where
    T: SortableWithContext<'s, A>,
    A: Accessor<Context = C> + 's,
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
impl<'s, A, C, T> Meta<'s, Proxy<T>> for ProxyMeta<A>
where
    A: Accessor<Context = C> + 's,
    T: SortableWithContext<'s, A>,
{
    fn accept_visitor<V: SortVisitor<'s, Target = Proxy<T>>>(&self, visitor: &mut V)
    where
        Self: Sized,
    {
        let mut v = ProxyVisitor {
            parent: visitor,
            access: self.access.clone(),
        };
        T::get_meta(self.access.clone()).accept_visitor(&mut v);
    }
}

struct ProxyVisitor<'a, P, A> {
    parent: &'a mut P,
    access: A,
}

#[persian_rug::constraints(context = C, access(R))]
impl<'a, 's, A, C, P, R> SortVisitor<'s> for ProxyVisitor<'a, P, A>
where
    A: Accessor<Context = C> + 's,
    P: SortVisitor<'s, Target = Proxy<R>>,
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
            &ProxyField {
                inner: field.clone(),
                access: self.access.clone(),
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
            &ProxyField {
                inner: field.clone(),
                access: self.access.clone(),
            },
            sort_key,
            meta,
        );
    }
}

#[derive(Clone)]
struct ProxyField<F, A> {
    inner: F,
    access: A,
}

#[persian_rug::constraints(context=C, access(R))]
impl<A, C, R, F, T> Field<Proxy<R>> for ProxyField<F, A>
where
    A: Accessor<Context = C>,
    F: Field<R, Value = T>,
{
    type Value = T;
    fn apply_sorter<V: Sorter<Self::Value>>(&self, op: &V, a: &Proxy<R>, b: &Proxy<R>) -> Ordering {
        self.inner.apply_sorter(
            &ProxyOp { parent: op },
            self.access.get(a),
            self.access.get(b),
        )
    }
}

struct ProxyOp<'a, V> {
    parent: &'a V,
}

impl<'a, V, T> Sorter<T> for ProxyOp<'a, V>
where
    V: Sorter<T>,
{
    fn compare(&self, a: &T, b: &T) -> Ordering {
        self.parent.compare(a, b)
    }
}

pub use django_query_derive::SortableWithPersianRug;
