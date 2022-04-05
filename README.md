# django-query

A set of tools for creating Django-style mock endpoints. The basic
usage is to annotate the type that you want to simulate having a
database of, derive the necessary traits, and then create an
[`Endpoint`](https://docs.rs/django-query/latest/django-query/struct.Endpoint.html)
to serve a sortable, filterable database of them.

Example usage:
```rust
#[derive(Filterable, IntoRow, Sortable)]
struct Foo {
    #[django(sort, op(lt, gt))]
    a: i32,
    #[django(sort, op(icontains, iexact))]
    b: String,
}

#[tokio::test]
fn simple_test() {
    let server = MockServer::start().await;
    
    // create a Vec of struct Foo here, and optionally 
    // wrap it in an Arc. 
    
    // Now serve our mock database with wiremock
    Mock::given(matchers::method("GET"))
         .respond_with(Endpoint::new(foo_table, Some(&server.uri())));
    
    // And now we can make requests from our code like
    reqwest::get(&format!("{}?limit=5&offset=5&b__icontains=apple&ordering=-a"))
       .await
       .expect("error getting response")
       .json()
       .await
       .expect("error parsing response");
    // which will perform
    //  - pagination (5 responses, offset by 5)
    //  - filtering (only array members whose b member contains apple,
    //    case insensitively)
    //  - sorting (the results will be in decreasing order of the a
    //    member)
}
```

## License

This code is made available under either an
[Apache-2.0](https://opensource.org/licenses/Apache-2.0) or an [MIT
license](https://opensource.org/licenses/MIT).
