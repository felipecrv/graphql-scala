package rs.felipe.graphql.language

import scala.collection.mutable.ArrayBuffer

object AST {
  type NodeKind = String

  /**
   * Contains a range of UTF-8 character offsets that identify
   * the region of the source from which the AST derived.
   */
  case class Location(start: Int, end: Int, source: Option[Source] = None)

  /**
   * The root class of all AST node types.
   *
   *   Node (abstract)
   *       Type (abstract)
   *           Name
   *       Document
   *       Definition (abstract)
   *           OperationDefinition
   *       VariableDefinition -
   *       Value (abstract)
   *           Variable
   *       SelectionSet
   *       Selection (abstract)
   *           Field
   *       Argument
   *       Selection (abstract)
   *           FragmentSpread
   *           InlineFragment
   *       Definition (abstract)
   *           FragmentDefinition
   *       Value (abstract)
   *           IntValue
   *           FloatValue
   *           StringValue
   *           BooleanValue
   *           EnumValue
   *           ArrayValue
   *           ObjectValue
   *       ObjectField
   *       Directive
   *       Type (abstract)
   *           ListType
   *           NonNullType
   */
  abstract class Node(val kind: NodeKind) {
    val loc: Option[Location]
  }


  // Name

  case class Name(value: String, loc: Option[Location]) extends Type("Name")


  // Document

  case class Document(definitions: ArrayBuffer[Definition], loc: Option[Location]) extends Node("Document")

  abstract class Definition(
    kind: NodeKind,
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: SelectionSet) extends Node(kind)

  abstract class Operation
  object Query extends Operation
  object Mutation extends Operation

  object Operation {
    def apply(s: String): Operation = {
      s match {
      case "query" => Query
      case "mutation" => Mutation
    }}
  }

  case class OperationDefinition(
    operation: Operation,
    name: Option[Name],
    variableDefinitions: Option[ArrayBuffer[VariableDefinition]],
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: SelectionSet,
    loc: Option[Location]) extends Definition("OperationDefinition", directives, selectionSet)

  case class VariableDefinition(
    variable: Variable,
    _type: Type,
    defaultValue: Option[Value],
    loc: Option[Location]) extends Node("VariableDefinition")

  case class Variable(name: Name, loc: Option[Location]) extends Value("Variable")

  case class SelectionSet(selections: ArrayBuffer[Selection], loc: Option[Location]) extends Node("SelectionSet")

  abstract class Selection(
    kind: NodeKind,
    directives: Option[ArrayBuffer[Directive]]) extends Node(kind)

  case class Field(
    alias: Option[Name],
    name: Name,
    arguments: Option[ArrayBuffer[Argument]],
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: Option[SelectionSet],
    loc: Option[Location]) extends Selection("Field", directives)

  case class Argument(
    name: Name,
    value: Value,
    loc: Option[Location]) extends Node("Argument")


  // Fragments

  case class FragmentSpread(
    name: Name,
    directives: Option[ArrayBuffer[Directive]],
    loc: Option[Location]) extends Selection("FragmentSpread", directives)

  case class InlineFragment(
    typeCondition: Name,
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: SelectionSet,
    loc: Option[Location]) extends Selection("InlineFragment", directives)

  case class FragmentDefinition(
    name: Name,
    typeCondition: Name,
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: SelectionSet,
    loc: Option[Location]) extends Definition("FragmentDefinition", directives, selectionSet)


  // Values

  abstract class Value(kind: NodeKind) extends Node(kind)

  case class IntValue(
    value: String,
    loc: Option[Location]) extends Value("IntValue")

  case class FloatValue(
    value: String,
    loc: Option[Location]) extends Value("FloatValue")

  case class StringValue(value: String, loc: Option[Location]) extends Value("StringValue")

  case class BooleanValue(value: Boolean, loc: Option[Location]) extends Value("BooleanValue")

  case class EnumValue(value: String, loc: Option[Location]) extends Value("EnumValue")

  case class ArrayValue(values: ArrayBuffer[Value], loc: Option[Location]) extends Value("ArrayValue")

  case class ObjectValue(fields: ArrayBuffer[ObjectField], loc: Option[Location]) extends Value("ObjectValue")

  case class ObjectField(
    name: Name,
    value: Value,
    loc: Option[Location]) extends Node("ObjectField")


  // Directives

  case class Directive(
    name: Name,
    arguments: Option[ArrayBuffer[Argument]],
    loc: Option[Location]) extends Node("Directive")


  // Types

  abstract class Type(kind: NodeKind) extends Node(kind)

  // Name is also a subclass of Type

  case class ListType(_type: Type, loc: Option[Location]) extends Type("ListType")

  case class NonNullType(_type: Either[Name, ListType], loc: Option[Location]) extends Type("NonNullType")
}
