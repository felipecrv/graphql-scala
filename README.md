# GraphQL-Scala

This is a Scala port from the JavaScript code of the [reference implementation
of GraphQL.](http://github.com/graphql/graphql-js)

GraphQL is a query language created by Facebook for describing data requirements
on complex application data models.

[![Build Status](https://travis-ci.org/philix/graphql-scala.svg)](https://travis-ci.org/philix/graphql-scala)
[![General GraphQL Slack Discussion](https://graphql-slack.herokuapp.com/badge.svg)](https://graphql-slack.herokuapp.com/)

## Code

GraphQL-Scala follows the code structure of
[graphql-js](http://github.com/graphql/graphql-js) and is also trying to provide
base libraries in Scala that would provide the bases for full GraphQL
implementations and tools. It is not a fully standalone GraphQL server that a
client developer could use to start manipulating and querying data. Most
importantly, it still does not provide a mapping to a functioning,
production-ready backend.

The parser is implemented "by hand" without any parser generator as a
dependency.

For a more comprehensive implementation of GraphQL tools in Scala check
[sangria-graphql/sangria](https://github.com/sangria-graphql/sangria).

## Getting Started

An overview of GraphQL in general is available in the
[README](https://github.com/facebook/graphql/blob/master/README.md)
for the
[Specification for GraphQL](https://github.com/facebook/graphql). That overview
describes a simple set of GraphQL examples.

### Using GraphQL-Scala

Download the source code and build using [SBT](http://www.scala-sbt.org/).

TODO(philix): publish on Maven

TODO(philix): implement schema building capabilities and document it here

### Contributing

We actively welcome pull requests, learn how to
[contribute](https://github.com/philix/graphql-scala/blob/master/CONTRIBUTING.md).

### Changelog

Changes are tracked as [Github releases](https://github.com/philix/graphql-scala/releases).

### License

GraphQL-Scala is [BSD-licensed](https://github.com/philix/graphql-scala/blob/master/LICENSE).
