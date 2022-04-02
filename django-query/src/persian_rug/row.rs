use crate::row::{AsForeignKeyWithContext, CellReducer, IntoRowWithContext, Serializer};
use crate::row::{CellValue, CellVisitor, ColumnVisitor};

use persian_rug::{Accessor, Proxy};

#[persian_rug::constraints(context = C, access(T))]
impl<'a, A, C, T> IntoRowWithContext<'a, A> for Proxy<T>
where
    A: Accessor<Context = C> + 'a,
    T: IntoRowWithContext<'a, A>,
{
    type Serializer = ProxySerializer<A>;
    fn get_serializer(access: A) -> Self::Serializer {
        ProxySerializer { access }
    }
}

pub struct ProxySerializer<A> {
    access: A,
}

#[persian_rug::constraints(context = C, access(T))]
impl<'r, A, C, T> Serializer<'r, Proxy<T>> for ProxySerializer<A>
where
    A: Accessor<Context = C> + 'r,
    T: IntoRowWithContext<'r, A>,
{
    fn accept_cell_visitor<V: CellVisitor>(&self, value: &Proxy<T>, visitor: &mut V) {
        T::get_serializer(self.access.clone()).accept_cell_visitor(self.access.get(value), visitor);
    }

    fn accept_column_visitor<V: ColumnVisitor>(&self, visitor: &mut V) {
        T::get_serializer(self.access.clone()).accept_column_visitor(visitor);
    }
}

#[persian_rug::constraints(context = C, access(T))]
impl<'a, 's, A, C, T> AsForeignKeyWithContext<'a, 's, A> for Proxy<T>
where
    T: AsForeignKeyWithContext<'a, 's, A>,
    A: Accessor<Context = C> + 's,
{
    type CellReducer = ProxyCellReducer<<T as AsForeignKeyWithContext<'a, 's, A>>::CellReducer, A>;
    fn get_cell_reducer(access: A, key: &'a str) -> Self::CellReducer {
        ProxyCellReducer {
            access: access.clone(),
            nested: T::get_cell_reducer(access, key),
        }
    }
}

pub struct ProxyCellReducer<F, A> {
    nested: F,
    access: A,
}

#[persian_rug::constraints(context = C, access(T))]
impl<'a, 'r, A, T, C, F> CellReducer<'a, 'r, Proxy<T>> for ProxyCellReducer<F, A>
where
    A: Accessor<Context = C>,
    F: CellReducer<'a, 'r, T>,
{
    fn reduce_to_cell(&self, value: &Proxy<T>) -> CellValue {
        self.nested.reduce_to_cell(self.access.get(value))
    }
}

// pub struct ProxySerializerWithContext<A> {
//     access: A
// }

// impl<'s, T, A> Serializer<'s, persian_rug::Proxy<T>> for ProxySerializerWithContext<A>
// where
//     T: IntoRowWithContext<'s, A>,
//     A: 's + Clone
// {
//     fn accept_cell_visitor<V: CellVisitor>(&self, value: &persian_rug::Proxy<T>, visitor: &mut V) {
//         T::get_serializer(self.access.clone()).accept_cell_visitor(self.access.get(value), visitor);
//     }

//     fn accept_column_visitor<V: ColumnVisitor>(&self, visitor: &mut V) {
//         T::get_serializer(self.access.clone()).accept_column_visitor(visitor);
//     }
// }

pub use django_query_derive::IntoRowWithPersianRug;
