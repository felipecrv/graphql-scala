package rs.felipe.graphql.language

import scala.collection.mutable.ArrayBuffer

import org.scalatest._
import Matchers._

import rs.felipe.graphql.language.AST._
import rs.felipe.graphql.error.GraphQLError
import rs.felipe.graphql.language.Parser._

class ParserSpec extends FunSpec {
  describe("Parser") {
    it("parse provides useful errors") {

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
        startWith("Syntax Error MyQuery.graphql (1:6) Expected Name, found EOF")
    }

    it("parses variable inline values") {
      parse("{ field(complex: { a: { b: [ $var ] } }) }")
    }

    it("parses constant default values") {
      (the [GraphQLError] thrownBy parse("query Foo($x: Complex = { a: { b: [ $var ] } }) { field }")).message should
      startWith("Syntax Error GraphQL (1:37) Unexpected $")
    }

    it("duplicate keys in input object is syntax error") {
      (the [GraphQLError] thrownBy parse("{ field(arg: { a: 1, a: 2 }) }")).message should
        startWith("Syntax Error GraphQL (1:22) Duplicate input object field a.")
    }

    it("parses kitchen sink") {
      val kitchenSinkURL = getClass.getResource("/kitchen-sink.graphql")
      val kitchenSink = scala.io.Source.fromURL(kitchenSinkURL).mkString
      parse(Source(kitchenSink))
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
