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
   *           ListValue
   *           ObjectValue
   *       ObjectField
   *       Directive
   *       Type (abstract)
   *           ListType
   *           NonNullType
   *       Definition (abstract)
   *         TypeDefinition (abstract)
   *           ObjectTypeDefinition
   *           InterfaceTypeDefinition
   *           UnionTypeDefinition
   *           ScalarTypeDefinition
   *           EnumTypeDefinition
   *           InputObjectTypeDefinition
   *           TypeExtensionDefinition
   *        FieldDefinition
   *        InputValueDefinition
   */
  sealed abstract class Node(val kind: NodeKind) {
    val loc: Option[Location]
  }


  // Name

  case class Name(value: String, loc: Option[Location]) extends Type("Name")


  // Document

  case class Document(definitions: ArrayBuffer[Definition], loc: Option[Location]) extends Node("Document")

  sealed abstract class Definition(kind: NodeKind) extends Node(kind)

  sealed trait Operation
  case object Query extends Operation
  case object Mutation extends Operation
  case object Subscription extends Operation

  object Operation {
    def apply(s: String): Operation = s match {
      case "query" => Query
      case "mutation" => Mutation
      case "subscription" => Subscription
    }
  }

  case class OperationDefinition(
    operation: Operation,
    name: Option[Name],
    variableDefinitions: Option[ArrayBuffer[VariableDefinition]],
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: SelectionSet,
    loc: Option[Location]) extends Definition("OperationDefinition")

  case class VariableDefinition(
    variable: Variable,
    _type: Type,
    defaultValue: Option[Value],
    loc: Option[Location]) extends Node("VariableDefinition")

  case class Variable(name: Name, loc: Option[Location]) extends Value("Variable")

  case class SelectionSet(
    selections: ArrayBuffer[Selection],
    loc: Option[Location]
  ) extends Node("SelectionSet")

  sealed abstract class Selection(
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
    typeCondition: Option[NamedType],
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: SelectionSet,
    loc: Option[Location]) extends Selection("InlineFragment", directives)

  case class FragmentDefinition(
    name: Name,
    typeCondition: NamedType,
    directives: Option[ArrayBuffer[Directive]],
    selectionSet: SelectionSet,
    loc: Option[Location]) extends Definition("FragmentDefinition")


  // Values

  sealed abstract class Value(kind: NodeKind) extends Node(kind)

  case class IntValue(
    value: String,
    loc: Option[Location]) extends Value("IntValue")

  case class FloatValue(
    value: String,
    loc: Option[Location]) extends Value("FloatValue")

  case class StringValue(value: String, loc: Option[Location]) extends Value("StringValue")

  case class BooleanValue(value: Boolean, loc: Option[Location]) extends Value("BooleanValue")

  case class EnumValue(value: String, loc: Option[Location]) extends Value("EnumValue")

  case class ListValue(values: ArrayBuffer[Value], loc: Option[Location]) extends Value("ListValue")

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


  // Type Reference

  sealed abstract class Type(kind: NodeKind) extends Node(kind)

  case class NamedType(name: Name, loc: Option[Location]) extends Type("NamedType")

  case class ListType(_type: Type, loc: Option[Location]) extends Type("ListType")

  case class NonNullType(_type: Either[NamedType, ListType], loc: Option[Location]) extends Type("NonNullType")

  // Types

  sealed abstract class TypeDefinition(kind: NodeKind) extends Definition(kind)

  case class ObjectTypeDefinition(
    name: Name,
    interfaces: Option[ArrayBuffer[NamedType]],
    fields: ArrayBuffer[FieldDefinition],
    loc: Option[Location]
  ) extends TypeDefinition("ObjectTypeDefinition")

  case class FieldDefinition(
    name: Name,
    arguments: ArrayBuffer[InputValueDefinition],
    _type: Type,
    loc: Option[Location]
  ) extends Node("FieldDefinition")

  case class InputValueDefinition(
    name: Name,
    _type: Type,
    defaultValue: Option[Value],
    loc: Option[Location]
  ) extends Node("InputValueDefinition")

  case class InterfaceTypeDefinition(
    name: Name,
    fields: ArrayBuffer[FieldDefinition],
    loc: Option[Location]
  ) extends TypeDefinition("InterfaceTypeDefinition")

  case class UnionTypeDefinition(
    name: Name,
    types: ArrayBuffer[NamedType],
    loc: Option[Location]
  ) extends TypeDefinition("UnionTypeDefinition")

  case class ScalarTypeDefinition(
    name: Name,
    loc: Option[Location]
  ) extends TypeDefinition("ScalarTypeDefinition")

  case class EnumTypeDefinition(
    name: Name,
    values: ArrayBuffer[EnumValueDefinition],
    loc: Option[Location]
  ) extends TypeDefinition("EnumTypeDefinition")

  case class EnumValueDefinition(
    name: Name,
    loc: Option[Location]
  ) extends Node("EnumValueDefinition")

  case class InputObjectTypeDefinition(
    name: Name,
    fields: ArrayBuffer[InputValueDefinition],
    loc: Option[Location]
  ) extends TypeDefinition("InputObjectTypeDefinition")

  case class TypeExtensionDefinition(
    definition: ObjectTypeDefinition,
    loc: Option[Location]
  ) extends TypeDefinition("TypeExtensionDefinition")
}
