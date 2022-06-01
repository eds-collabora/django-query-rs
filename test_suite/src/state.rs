#![cfg(test)]

use django_query::filtering::FilterableWithPersianRug;
use django_query::row::{IntoRowWithContext, IntoRowWithPersianRug, Serializer};
use django_query::sorting::SortableWithPersianRug;

#[derive(IntoRowWithPersianRug, SortableWithPersianRug, FilterableWithPersianRug)]
#[django(persian_rug(context = C, access(Foo<C>)))]
#[persian_rug::contextual(C)]
struct Foo<C: persian_rug::Context> {
    #[django(exclude)]
    _marker: core::marker::PhantomData<C>,
    #[django(sort, op(lt, gt))]
    a: i32,
}

#[derive(IntoRowWithPersianRug, SortableWithPersianRug, FilterableWithPersianRug)]
#[django(persian_rug(context = C, access(Foo<C>, Bar<C>)))]
#[persian_rug::contextual(C)]
struct Bar<C: persian_rug::Context> {
    #[django(sort, op(lt, gt))]
    a: i32,
    #[django(sort("a"), foreign_key = "a", traverse)]
    foo: persian_rug::Proxy<Foo<C>>,
}

#[derive(IntoRowWithPersianRug, SortableWithPersianRug, FilterableWithPersianRug)]
#[django(persian_rug(context = C, access(Foo<C>, Bar<C>, Baz<C>)))]
#[persian_rug::contextual(C)]
struct Baz<C: persian_rug::Context> {
    #[django(sort, op(lt, gt))]
    a: i32,
    #[django(sort("foo"), foreign_key = "a", traverse)]
    bar: persian_rug::Proxy<Bar<C>>,
}

#[derive(Default)]
#[persian_rug::persian_rug]
struct State {
    #[table]
    foo: Foo<State>,
    #[table]
    bar: Bar<State>,
    #[table]
    baz: Baz<State>,
}

#[test]
fn test_into_row() {
    use persian_rug::Owner;

    let mut s: State = Default::default();

    let f1 = s.add(Foo {
        a: 1,
        _marker: Default::default(),
    });
    let b1 = s.add(Bar { a: 2, foo: f1 });
    let z1 = s.add(Baz { a: 3, bar: b1 });

    println!(
        "{}",
        persian_rug::Proxy::<Foo<State>>::get_serializer(&s).to_json(&f1)
    );
    println!(
        "{}",
        persian_rug::Proxy::<Bar<State>>::get_serializer(&s).to_json(&b1)
    );
    println!(
        "{}",
        persian_rug::Proxy::<Baz<State>>::get_serializer(&s).to_json(&z1)
    );
}

#[test]
fn test_ordering() {
    use persian_rug::Owner;

    let mut s: State = Default::default();

    let f1 = s.add(Foo {
        a: 1,
        _marker: Default::default(),
    });

    let f2 = s.add(Foo {
        a: 2,
        _marker: Default::default(),
    });
    let f3 = s.add(Foo {
        a: 3,
        _marker: Default::default(),
    });
    let f4 = s.add(Foo {
        a: 4,
        _marker: Default::default(),
    });
    let f5 = s.add(Foo {
        a: 5,
        _marker: Default::default(),
    });

    let b1 = s.add(Bar { a: 2, foo: f1 });
    let b2 = s.add(Bar { a: 1, foo: f2 });
    let b3 = s.add(Bar { a: 3, foo: f3 });
    let b4 = s.add(Bar { a: 4, foo: f4 });
    let b5 = s.add(Bar { a: 5, foo: f5 });

    s.add(Baz { a: 3, bar: b1 });
    s.add(Baz { a: 1, bar: b2 });
    s.add(Baz { a: 2, bar: b3 });
    s.add(Baz { a: 4, bar: b4 });
    s.add(Baz { a: 5, bar: b5 });

    let os =
        ::django_query::sorting::OrderingSetWithContext::<persian_rug::Proxy<Foo<State>>, _>::new(
            &s,
        );
    let mut foos = s.get_proxy_iter().copied().collect();
    let sort = os.create_sort("a").unwrap();
    sort.sort_vec(&mut foos);
    println!("Foos: {:?}", foos);

    let os =
        ::django_query::sorting::OrderingSetWithContext::<persian_rug::Proxy<Bar<State>>, _>::new(
            &s,
        );
    let mut bars = s.get_proxy_iter().copied().collect();
    let sort = os.create_sort("a").unwrap();
    sort.sort_vec(&mut bars);
    println!("Bars (by a): {:?}", bars);
    let sort = os.create_sort("foo").unwrap();
    sort.sort_vec(&mut bars);
    println!("Bars (by foo): {:?}", bars);

    let os =
        ::django_query::sorting::OrderingSetWithContext::<persian_rug::Proxy<Baz<State>>, _>::new(
            &s,
        );
    let mut bazs = s.get_proxy_iter().copied().collect();
    let sort = os.create_sort("a").unwrap();
    sort.sort_vec(&mut bazs);
    println!("Bazs (by a): {:?}", bazs);
    let sort = os.create_sort("bar").unwrap();
    sort.sort_vec(&mut bazs);
    println!("Bazs (by bar): {:?}", bazs);
}

#[test]
fn test_filtering() {
    use persian_rug::Owner;

    let mut s: State = Default::default();

    let f1 = s.add(Foo {
        a: 1,
        _marker: Default::default(),
    });

    let f2 = s.add(Foo {
        a: 2,
        _marker: Default::default(),
    });
    let f3 = s.add(Foo {
        a: 3,
        _marker: Default::default(),
    });
    let f4 = s.add(Foo {
        a: 4,
        _marker: Default::default(),
    });
    let f5 = s.add(Foo {
        a: 5,
        _marker: Default::default(),
    });

    let b1 = s.add(Bar { a: 2, foo: f1 });
    let b2 = s.add(Bar { a: 1, foo: f2 });
    let b3 = s.add(Bar { a: 3, foo: f3 });
    let b4 = s.add(Bar { a: 4, foo: f4 });
    let b5 = s.add(Bar { a: 5, foo: f5 });

    s.add(Baz { a: 3, bar: b1 });
    s.add(Baz { a: 1, bar: b2 });
    s.add(Baz { a: 2, bar: b3 });
    s.add(Baz { a: 4, bar: b4 });
    s.add(Baz { a: 5, bar: b5 });

    let os =
        ::django_query::filtering::OperatorSetWithContext::<persian_rug::Proxy<Foo<State>>, _>::new(
            &s,
        );
    let mut foos = s.get_proxy_iter().copied().collect();
    let filter = os.create_filter_from_query("a=3").unwrap();
    filter.filter_vec(&mut foos);
    println!("Foos(a=3): {:?}", foos);

    let mut foos = s.get_proxy_iter().copied().collect();
    let filter = os.create_filter_from_query("a__lt=3").unwrap();
    filter.filter_vec(&mut foos);
    println!("Foos(a<3): {:?}", foos);

    let os =
        ::django_query::filtering::OperatorSetWithContext::<persian_rug::Proxy<Bar<State>>, _>::new(
            &s,
        );

    let mut bars = s.get_proxy_iter().copied().collect();
    let filter = os.create_filter_from_query("foo__a=3").unwrap();
    filter.filter_vec(&mut bars);
    println!("Bars(foo.a=3): {:?}", bars);

    let mut bars = s.get_proxy_iter().copied().collect();
    let filter = os.create_filter_from_query("foo__a__lt=3").unwrap();
    filter.filter_vec(&mut bars);
    println!("Bars(foo.a<3): {:?}", bars);

    let os =
        ::django_query::filtering::OperatorSetWithContext::<persian_rug::Proxy<Baz<State>>, _>::new(
            &s,
        );
    let mut bazs = s.get_proxy_iter().copied().collect();
    let filter = os.create_filter_from_query("bar__foo__a=3").unwrap();
    filter.filter_vec(&mut bazs);
    println!("Bazs(bar.foo.a)=3: {:?}", bazs);

    let mut bazs = s.get_proxy_iter().copied().collect();
    let filter = os.create_filter_from_query("bar__foo__a__lt=3").unwrap();
    filter.filter_vec(&mut bazs);
    println!("Bazs(bar.foo.a<3): {:?}", bazs);
}
