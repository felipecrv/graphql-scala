Contributing to graphql-scala
==========================

We want to make contributing to this project as easy and transparent as
possible. Hopefully this document makes the process for contributing clear and
answers any questions you may have. If not, feel free to open an
[Issue](https://github.com/philix/graphql-scala/issues).

## Issues

We use GitHub issues to track public bugs and requests. Please ensure your bug
description is clear and has sufficient instructions to be able to reproduce the
issue.

## Pull Requests

All active development of graphql-scala happens on GitHub. We actively welcome
your [pull requests](https://help.github.com/articles/creating-a-pull-request).

### Considered Changes

Since graphql-scala strongly follows the code structure of
[graphql-js](http://github.com/graphql/graphql-js), only changes which follow
this structure and the [GraphQL spec](https://facebook.github.io/graphql/) will
be considered. If you have a change in mind which requires a change to the spec,
please first open an [issue](https://github.com/facebook/graphql/issues/)
against the spec.

### Getting Started

1. Fork this repo by using the "Fork" button in the upper-right

2. Check out your fork

   ```sh
   git clone git@github.com:yournamehere/graphql-scala.git
   ```

3. Install [SBT](http://www.scala-sbt.org/)

4. Download the dependencies and run the tests

   ```sh
   sbt test
   ```

5. Get coding! If you've added code, add tests. If you've changed APIs, update
   any relevant documentation or tests. Ensure your work is committed within a
   feature branch.

6. Ensure all tests are still passing

## Coding Style

* 2 spaces for indentation (no tabs)
* 120 character line length strongly preferred
* Use mutation with moderation
* Don't use semicolons
* Avd abbr wrds.

## License

By contributing to graphql-scala, you agree that your contributions will be
licensed under its BSD license.
