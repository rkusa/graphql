# graphql

[![Build Status](https://travis-ci.org/rkusa/graphql.svg?branch=master)](https://travis-ci.org/rkusa/graphql)

Experimental¹ [futures](https://github.com/alexcrichton/futures-rs)-based GraphQL server implementation.

Example http endpoint can be started with:

    cargo run --example http

(the server understands only very simple queries for now)

Send an example query:

    curl http://0.0.0.0:3000/ -i -X POST -H 'Content-Type: application/json' -d '{"query:"query{user{id}}"}'

¹ current main focus is on proof of concept and API ergonomics and not feature set for now
