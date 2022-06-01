#![cfg(test)]

use django_query::{filtering::Filterable, mock::Endpoint, row::IntoRow, sorting::Sortable};
use std::sync::Arc;

#[derive(Clone, Filterable, Sortable, IntoRow)]
struct Foo {
    #[django(sort)]
    name: String,
}

#[tokio::test]
async fn test_vec() {
    let server = wiremock::MockServer::start().await;

    let foos = vec![
        Foo {
            name: "hello".to_string(),
        },
        Foo {
            name: "goodbye".to_string(),
        },
    ];

    wiremock::Mock::given(wiremock::matchers::method("GET"))
        .and(wiremock::matchers::path("/foos/"))
        .respond_with(Endpoint::new(foos, Some(&server.uri())))
        .mount(&server)
        .await;
}

#[tokio::test]
async fn test_arc() {
    let server = wiremock::MockServer::start().await;

    let foos: Arc<Vec<_>> = Arc::new(vec![
        Foo {
            name: "hello".to_string(),
        },
        Foo {
            name: "goodbye".to_string(),
        },
    ]);

    wiremock::Mock::given(wiremock::matchers::method("GET"))
        .and(wiremock::matchers::path("/foos/"))
        .respond_with(Endpoint::new(foos, Some(&server.uri())))
        .mount(&server)
        .await;
}
