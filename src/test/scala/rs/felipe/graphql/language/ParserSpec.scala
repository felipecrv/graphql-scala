package rs.felipe.graphql.language

import scala.collection.mutable.ArrayBuffer

import org.scalatest._
import Matchers._

import rs.felipe.graphql.language.AST._
import rs.felipe.graphql.error.GraphQLError
import rs.felipe.graphql.language.Parser._

class ParserSpec extends FunSpec {
  describe("Parser") {
    it("acepts option to not include source") {
      parse(Source("{ field }"), ParseOptions(false, true)) should matchPattern {
        case Document(_, Some(Location(_, _, None))) =>
      }
    }

    it("parse provides useful errors") {

      val caughtError = ({
        try {
          parse("{")
          None
        } catch {
          case error: GraphQLError => Some(error)
        }
      }).get

      caughtError.message should startWith("Syntax Error GraphQL (1:2) Expected Name, found EOF")
      caughtError.positions.get.length shouldBe(1)
      caughtError.positions.get(0) shouldBe(1)

      caughtError.locations.get.length shouldBe(1)
      caughtError.locations.get(0) should matchPattern {
        case SourceLocation(1, 2) =>
      }

      (the [GraphQLError] thrownBy parse("""  { ...MissingOn }
                                           |  fragment MissingOn Type
                                           |""".stripMargin)).message should
        startWith("Syntax Error GraphQL (2:22) Expected \"on\", found Name \"Type\"")

      (the [GraphQLError] thrownBy parse("{ field: {} }")).message should
        startWith("Syntax Error GraphQL (1:10) Expected Name, found {")

      (the [GraphQLError] thrownBy parse("notanoperation Foo { field }")).message should
        startWith("Syntax Error GraphQL (1:1) Unexpected Name \"notanoperation\"")

      (the [GraphQLError] thrownBy parse("...")).message should
        startWith("Syntax Error GraphQL (1:1) Unexpected ...")

    }

    it("parse provides useful error when using source") {

      (the [GraphQLError] thrownBy parse(Source("query", "MyQuery.graphql"))).message should
        startWith("Syntax Error MyQuery.graphql (1:6) Expected {, found EOF")
    }

    it("parses variable inline values") {
      parse("{ field(complex: { a: { b: [ $var ] } }) }")
    }

    it("parses constant default values") {
      (the [GraphQLError] thrownBy parse("query Foo($x: Complex = { a: { b: [ $var ] } }) { field }")).message should
      startWith("Syntax Error GraphQL (1:37) Unexpected $")
    }

    it("does not accept fragments named \"on\"") {
      (the [GraphQLError] thrownBy parse("fragment on on on { on }")).message should
        startWith("Syntax Error GraphQL (1:10) Unexpected Name \"on\"")
    }

    it("does not accept fragments spread of \"on\"") {
      (the [GraphQLError] thrownBy parse("{ ...on }")).message should
        startWith("Syntax Error GraphQL (1:9) Expected Name, found }")
    }

    it("does not allow null as value") {
      (the [GraphQLError] thrownBy parse("{ fieldWithNullableStringInput(input: null) }")).message
        startWith("Syntax Error GraphQL (1:39) Unexpected Name \"null\"")
    }

    it("parses multi-byte characters") {
      // Note: \u0A0A could be naively interpretted as two line-feed chars.
      val document = parse("""
        # This comment has a \u0A0A multi-byte character.
        { field(arg: "Has a \u0A0A multi-byte character.") }
      """)

      val definition = document.definitions.head.asInstanceOf[OperationDefinition]
      val selection = definition.selectionSet.selections.head
      val argument = selection.asInstanceOf[Field].arguments.get.head
      argument.value should matchPattern {
        case StringValue("Has a \u0A0A multi-byte character.", _) =>
      }
    }

    it("parses kitchen sink") {
      val kitchenSinkURL = getClass.getResource("/kitchen-sink.graphql")
      val kitchenSink = scala.io.Source.fromURL(kitchenSinkURL).mkString
      parse(Source(kitchenSink))
    }

    it("allows non-keywords anywhere a Name is allowed") {
      val nonKeywords = List(
        "on",
        "fragment",
        "query",
        "mutation",
        "true",
        "false"
      )
      nonKeywords foreach {
        keyword => {
          // You can't define or reference a fragment named `on`.
          val fragmentName = if (keyword === "on") "a" else keyword

        parse(s"""query ${keyword} {
  ... $fragmentName
  ... on $keyword { field }
}
fragment $fragmentName on Type {
  $keyword($keyword: $$keyword) @$keyword($keyword: $keyword)
}""")
        }
      }
    }

    it("parses experimental subscription feature") {
      parse("""
        subscription Foo {
          subscriptionField
        }
      """)
    }

    it("parses anonymous operations") {
      parse("""
        mutation {
          mutationField
        }
      """)
    }

    it("parse creates ast") {

      val source = Source("""{
                            |    node(id: 4) {
                            |      id,
                            |      name
                            |    }
                            |  }
                            |  """.stripMargin)
      val document = parse(source)

      document should matchPattern {
        case Document(definitions, Some(Location(0, 53, Some(_)))) =>
      }

      val definition = document.definitions.head.asInstanceOf[OperationDefinition]
      definition should matchPattern {
        case OperationDefinition(Query, None, None, None, selectionSet, Some(Location(0, 50, _))) =>
      }

      // the "node" selection
      val nodeSelection = definition.selectionSet.selections.head.asInstanceOf[Field]
      nodeSelection should matchPattern {
        case Field(None, Name("node", _), arguments, _, Some(SelectionSet(_, _)), location) =>
      }

      // the "node" selection argument
      val nodeSelectionArgument: Argument = nodeSelection.arguments.get.head
      nodeSelectionArgument should matchPattern {
        case Argument(Name("id", _), IntValue("4", _), _) =>
      }

      // the "node" selectionSet
      val nodeSelectionSetSelections = nodeSelection.selectionSet.get.selections
      nodeSelectionSetSelections(0) should matchPattern {
        case Field(None, Name("id", _), _, _, None, _) =>
      }
      nodeSelectionSetSelections(1) should matchPattern {
        case Field(None, Name("name", _), _, _, None, _) =>
      }
    }
  }
}
