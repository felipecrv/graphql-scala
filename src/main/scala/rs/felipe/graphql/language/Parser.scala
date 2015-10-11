package rs.felipe.graphql.language

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import rs.felipe.graphql.language.AST._
import rs.felipe.graphql.language.Lexer._
import rs.felipe.graphql.error.GraphQLError
import rs.felipe.graphql.error.GraphQLError.syntaxError

object Parser {
  /**
   * Configuration options to control parser behavior
   *
   * By default, the parser creates AST nodes that know the location
   * in the source that they correspond to. This noLocation configuration
   * flag disables that behavior for performance or testing.
   *
   * By default, the parser creates AST nodes that contain a reference
   * to the source that they were created from. This noSource configuration
   * flag disables that behavior for performance or testing.
   */
  case class ParseOptions(noLocation: Boolean = false, noSource: Boolean = false)

  /**
   * The parser object that is used to store state throughout the process of parsing.
   */
  case class Parser(val source: Source, val options: ParseOptions) {
    private val lexToken = {
      val token = Lexer.lex(source)
      token
    }

    // The mutable state of the parser
    private var token = lexToken(None)
    private var prevEnd = 0

    /**
     * Returns a location object, used to identify the place in
     * the source that created a given parsed object.
     */
    private def loc(start: Int): Option[Location] = {
      if (options.noLocation) {
        None
      } else if (options.noSource) {
        Some(Location(start, prevEnd))
      } else {
        Some(Location(start, prevEnd, Some(source)))
      }
    }

    /**
     * Moves the internal parser object to the next lexed token.
     */
    def advance = {
      prevEnd = token.end
      token = lexToken(Some(prevEnd))
    }

    /**
     * Determines if the next token is of a given kind
     */
    def peek(kind: TokenKind): Boolean = token.kind == kind

    /**
     * If the next token is of the given kind, return true after advancing
     * the parser. Otherwise, do not change the parser state and return false.
     */
    def skip(kind: TokenKind): Boolean = {
      val matched = token.kind == kind
      if (matched) {
        advance
      }
      matched
    }

    /**
     * If the next token is of the given kind, return that token after advancing
     * the parser. Otherwise, do not change the parser state and return false.
     */
    def expect(kind: TokenKind): Token = {
      if (token.kind == kind) {
        val _token = token
        advance
        _token
      } else {
        throw syntaxError(
          source,
          token.start,
          s"Expected ${getTokenKindDesc(kind)}, found ${token.getTokenDesc}")
      }
    }

    /**
     * If the next token is a keyword with the given value, return that token after
     * advancing the parser. Otherwise, do not change the parser state and return
     * false.
     */
    def expectKeyword(value: String): Token = {
      if (token.kind == TokenKind.NAME && token.value.map(_ == value).getOrElse(false)) {
        val _token = token
        advance
        _token
      } else {
        throw syntaxError(
          source,
          token.start,
          s"""Expected "${value}", found ${token.getTokenDesc}""")
      }
    }

    /**
     * Helper function for creating an error when an unexpected lexed token
     * is encountered.
     */
    private def unexpected(atToken: Option[Token] = None): GraphQLError = {
      val _token = atToken.getOrElse(token)
      syntaxError(source, token.start, s"Unexpected ${_token.getTokenDesc}")
    }

    type ParseFn[T] = Parser => T

    /**
     * Returns a possibly empty list of parse nodes, determined by
     * the parseFn. This list begins with a lex token of openKind
     * and ends with a lex token of closeKind. Advances the parser
     * to the next lex token after the closing token.
     */
    def any[T](openKind: TokenKind, parseFn: ParseFn[T], closeKind: TokenKind): ArrayBuffer[T] = {
      expect(openKind)
      val nodes = ArrayBuffer[T]()
      while (!skip(closeKind)) {
        val node = parseFn(this)
        nodes.append(node)
      }
      nodes
    }

    /**
     * Returns a non-empty list of parse nodes, determined by
     * the parseFn. This list begins with a lex token of openKind
     * and ends with a lex token of closeKind. Advances the parser
     * to the next lex token after the closing token.
     */
    def many[T](openKind: TokenKind, parseFn: ParseFn[T], closeKind: TokenKind): ArrayBuffer[T] = {
      expect(openKind)
      val nodes = new ArrayBuffer[T]
      nodes.append(parseFn(this))
      while (!skip(closeKind)) {
        nodes.append(parseFn(this))
      }
      nodes
    }

    /**
     * Converts a name lex token into a name parse node.
     */
    def parseName: Name = {
      val _token = expect(TokenKind.NAME)
      Name(_token.value.getOrElse(""), loc(token.start))
    }


    // Implements the parsing rules in the Document section.

    /**
     * Document : Definition+
     */
    def parseDocument: Document = {
      val start = token.start
      val definitions = ArrayBuffer[Definition]()
      do {
        definitions.append(parseDefinition)
      } while (!skip(TokenKind.EOF))

      Document(definitions, loc(start))
    }


    /**
     * Definition :
     *   - OperationDefinition
     *   - FragmentDefinition
     *   - TypeDefinition
     */
    def parseDefinition: Definition = {
      if (peek(TokenKind.BRACE_L)) {
        parseOperationDefinition
      } else if (peek(TokenKind.NAME)) {
        token.value.get match {
          // Note: subscription is an experimental non-spec addition.
          case "query" | "mutation" | "subscription" => parseOperationDefinition
          case "fragment" => parseFragmentDefinition
          case "type"
               | "interface"
               | "union"
               | "scalar"
               | "enum"
               | "input"
               | "extend" => parseTypeDefinition
          case _ => throw unexpected()
        }
      } else {
        throw unexpected()
      }
    }

    // Implements the parsing rules in the Operations section.

    /**
     * OperationDefinition :
     *  - SelectionSet
     *  - OperationType Name? VariableDefinitions? Directives? SelectionSet
     *
     * OperationType : one of query mutation
     */
    def parseOperationDefinition: OperationDefinition = {
      val start = token.start
      if (peek(TokenKind.BRACE_L)) {
        OperationDefinition(Query, None, None, None, parseSelectionSet, loc(start))
      } else {
        val operationToken = expect(TokenKind.NAME)
        val name = if (peek(TokenKind.NAME)) Some(parseName) else None
        OperationDefinition(
          Operation(operationToken.value.getOrElse("")),
          name,
          Some(parseVariableDefinitions),
          Some(parseDirectives),
          parseSelectionSet,
          loc(start))
      }
    }

    /**
     * VariableDefinitions : ( VariableDefinition+ )
     */
    def parseVariableDefinitions: ArrayBuffer[VariableDefinition] = {
      if (peek(TokenKind.PAREN_L))
        many[VariableDefinition](TokenKind.PAREN_L, _.parseVariableDefinition, TokenKind.PAREN_R)
      else ArrayBuffer[VariableDefinition]()
    }

    /**
     * VariableDefinition : Variable : Type DefaultValue?
     */
    def parseVariableDefinition: VariableDefinition = {
      val start = token.start
      val variable = parseVariable
      expect(TokenKind.COLON)
      val _type = parseType
      val defaultValue = if (skip(TokenKind.EQUALS)) Some(parseValueLiteral(true)) else None

      VariableDefinition(variable, _type, defaultValue, loc(start))
    }

    /**
     * Variable : $ Name
     */
    def parseVariable: Variable = {
      val start = token.start
      expect(TokenKind.DOLLAR)
      Variable(parseName, loc(start))
    }

    /**
     * SelectionSet : { Selection+ }
     */
    def parseSelectionSet: SelectionSet = {
      val start = token.start
      def selections = many[Selection](TokenKind.BRACE_L, _.parseSelection, TokenKind.BRACE_R)
      SelectionSet(selections, loc(start))
    }

    /**
     * Selection :
     *   - Field
     *   - FragmentSpread
     *   - InlineFragment
     */
    def parseSelection: Selection = {
      if (peek(TokenKind.SPREAD)) {
        val either = parseFragment
        either.left.getOrElse(either.right.get)
      } else {
        parseField
      }
    }

    /**
     * Field : Alias? Name Arguments? Directives? SelectionSet?
     *
     * Alias : Name :
     */
    def parseField: Field = {
      val start = token.start

      val nameOrAlias = parseName
      val (alias, name) =
        if (skip(TokenKind.COLON)) (Some(nameOrAlias), parseName)
        else (None, nameOrAlias)
      val arguments = parseArguments
      val directives = parseDirectives
      val selectionSet =
        if (peek(TokenKind.BRACE_L)) Some(parseSelectionSet)
        else None

      Field(alias, name, Some(arguments), Some(directives), selectionSet, loc(start))
    }

    /**
     * Arguments : ( Argument+ )
     */
    def parseArguments: ArrayBuffer[Argument] = {
      if (peek(TokenKind.PAREN_L)) many[Argument](TokenKind.PAREN_L, _.parseArgument, TokenKind.PAREN_R)
      else ArrayBuffer[Argument]()
    }

    /**
     * Argument : Name : Value
     */
    def parseArgument: Argument = {
      val start = token.start

      val name = parseName
      expect(TokenKind.COLON)
      val value = parseValueLiteral(false)
      Argument(name, value, loc(start))
    }


    // Implements the parsing rules in the Fragments section.

    /**
     * Corresponds to both FragmentSpread and InlineFragment in the spec.
     *
     * FragmentSpread : ... FragmentName Directives?
     *
     * InlineFragment : ... TypeCondition? Directives? SelectionSet
     */
    def parseFragment: Either[FragmentSpread, InlineFragment] = {
      val start = token.start
      expect(TokenKind.SPREAD)
      if (peek(TokenKind.NAME) && token.value.map(_ != "on").getOrElse(true)) {
        val name = parseFragmentName
        val directives = parseDirectives
        Left(FragmentSpread(name, Some(directives), loc(start)))
      } else {
        val typeCondition = if (token.value.map(_ == "on").getOrElse(false)) {
          advance
          Some(parseNamedType)
        } else {
          None
        }
        val directives = parseDirectives
        val selectionSet = parseSelectionSet
        Right(InlineFragment(typeCondition, Some(directives), selectionSet, loc(start)))
      }
    }

    /**
     * FragmentDefinition :
     *   - fragment FragmentName on TypeCondition Directives? SelectionSet
     *
     * TypeCondition : NamedType
     */
    def parseFragmentDefinition: FragmentDefinition = {
      val start = token.start

      expectKeyword("fragment")
      val name = parseFragmentName

      expectKeyword("on")
      val typeCondition = parseNamedType

      val directives = parseDirectives
      val selectionSet = parseSelectionSet

      FragmentDefinition(name, typeCondition, Some(directives), selectionSet, loc(start))
    }

    /**
     * FragmentName : Name but not `on`
     */
    def parseFragmentName: Name = {
      if (token.value.map(_ == "on").getOrElse(false)) {
        throw unexpected()
      }
      parseName
    }


    // Implements the parsing rules in the Values section.

    /**
     * Value[Const] :
     *   - [~Const] Variable
     *   - IntValue
     *   - FloatValue
     *   - StringValue
     *   - BooleanValue
     *   - EnumValue
     *   - ListValue[?Const]
     *   - ObjectValue[?Const]
     *
     * BooleanValue : one of `true` `false`
     *
     * EnumValue : Name but not `true`, `false` or `null`
     */
    def parseValueLiteral(isConst: Boolean): Value = {
      val _token = token
      val start = _token.start

      _token.kind match {
        case TokenKind.BRACKET_L => return parseList(isConst)
        case TokenKind.BRACE_L => return parseObject(isConst)
        case TokenKind.INT => {
          advance
          IntValue(_token.value.getOrElse("0"), loc(start))
        }
        case TokenKind.FLOAT => {
          advance
          FloatValue(_token.value.getOrElse("0.0"), loc(start))
        }
        case TokenKind.STRING => {
          advance
          StringValue(_token.value.getOrElse(""), loc(start))
        }
        case TokenKind.NAME => {
          _token.value match {
            case Some("true") => {
              advance
              BooleanValue(true, loc(start))
            }
            case Some("false") => {
              advance
              BooleanValue(false, loc(start))
            }
            case Some("null") => throw unexpected()
            case Some(value) => {
              advance
              EnumValue(value, loc(start))
            }
            case _ => throw unexpected()
          }
        }
        case TokenKind.DOLLAR => {
          if (!isConst) {
            parseVariable
          } else {
            throw unexpected()
          }
        }
        case _ => throw unexpected()
      }
    }

    def parseConstValue: Value = parseValueLiteral(true)

    def parseValueValue: Value = parseValueLiteral(false)

    /**
     * ListValue[Const] :
     *   - [ ]
     *   - [ Value[?Const]+ ]
     */
    def parseList(isConst: Boolean): ListValue = {
      val start = token.start
      val item: ParseFn[Value] = if (isConst) _.parseConstValue else _.parseValueValue
      val values = any(TokenKind.BRACKET_L, item, TokenKind.BRACKET_R)
      ListValue(values, loc(start))
    }

    /**
     * ObjectValue[Const] :
     *   - { }
     *   - { ObjectField[?Const]+ }
     */
    def parseObject(isConst: Boolean): ObjectValue = {
      val start = token.start
      expect(TokenKind.BRACE_L)
      val fields = ArrayBuffer[ObjectField]()
      while (!skip(TokenKind.BRACE_R)) {
        fields.append(parseObjectField(isConst))
      }
      ObjectValue(fields, loc(start))
    }

    /**
     * ObjectField[Const] : Name : Value[?Const]
     */
    def parseObjectField(isConst: Boolean): ObjectField = {
      val start = token.start

      val name = parseName
      expect(TokenKind.COLON)
      val value = parseValueLiteral(isConst)
      ObjectField(name, value, loc(start))
    }

    // Implements the parsing rules in the Directives section.

    /**
     * Directives : Directive+
     */
    def parseDirectives: ArrayBuffer[Directive] = {
      val directives = ArrayBuffer[Directive]()
      while (peek(TokenKind.AT)) {
        directives.append(parseDirective)
      }
      directives
    }

    /**
     * Directive : @ Name Arguments?
     */
    def parseDirective: Directive = {
      val start = token.start
      expect(TokenKind.AT)
      Directive(parseName, Some(parseArguments), loc(start))
    }


    // Implements the parsing rules in the Types section.

    /**
     * Type :
     *   - NamedType
     *   - ListType
     *   - NonNullType
     */
    def parseType: Type = {
      val start = token.start

      val _type =
        if (skip(TokenKind.BRACKET_L)) {
          val itemType = parseType
          expect(TokenKind.BRACKET_R)
          Right(ListType(itemType, loc(start)))
        } else {
          Left(parseNamedType)
        }

      if (skip(TokenKind.BANG)) {
        NonNullType(_type, loc(start))
      } else {
        _type.left.getOrElse(_type.right.get)
      }
    }

    /**
     * NamedType : Name
     */
    def parseNamedType: NamedType = {
      val start = token.start
      NamedType(parseName, loc(start))
    }


    // Implements the parsing rules in the Type Definition section.

    /**
     * TypeDefinition :
     *   - ObjectTypeDefinition
     *   - InterfaceTypeDefinition
     *   - UnionTypeDefinition
     *   - ScalarTypeDefinition
     *   - EnumTypeDefinition
     *   - InputObjectTypeDefinition
     *   - TypeExtensionDefinition
     */
    def parseTypeDefinition: TypeDefinition = {
      if (!peek(TokenKind.NAME)) {
        throw unexpected()
      } else {
        token.value.get match {
          case "type" => parseObjectTypeDefinition
          case "interface" => parseInterfaceTypeDefinition
          case "union" => parseUnionTypeDefinition
          case "scalar" => parseScalarTypeDefinition
          case "enum" => parseEnumTypeDefinition
          case "input" => parseInputObjectTypeDefinition
          case "extend" => parseTypeExtensionDefinition
          case _ => throw unexpected()
        }
      }
    }

    /**
     * ObjectTypeDefinition : type Name ImplementsInterfaces? { FieldDefinition+ }
     */
    def parseObjectTypeDefinition: ObjectTypeDefinition = {
      val start = token.start
      expectKeyword("type")
      val name = parseName
      val interfaces = parseImplementsInterfaces
      val fields = any(
        TokenKind.BRACE_L,
        _.parseFieldDefinition,
        TokenKind.BRACE_R
      )
      ObjectTypeDefinition(name, Some(interfaces), fields, loc(start))
    }

    /**
     * ImplementsInterfaces : implements NamedType+
     */
    def parseImplementsInterfaces: ArrayBuffer[NamedType] = {
      val types = ArrayBuffer[NamedType]()
      if (token.value.map( _ == "implements").getOrElse(false)) {
        advance
        do {
          types.append(parseNamedType)
        } while (!peek(TokenKind.BRACE_L))
      }
      types
    }

    /**
     * FieldDefinition : Name ArgumentsDefinition? : Type
     */
    def parseFieldDefinition: FieldDefinition = {
      val start = token.start
      val name = parseName
      val args = parseArgumentDefs
      expect(TokenKind.COLON)
      val _type = parseType
      FieldDefinition(name, args, _type, loc(start))
    }

    /**
     * ArgumentsDefinition : ( InputValueDefinition+ )
     */
    def parseArgumentDefs: ArrayBuffer[InputValueDefinition] = {
      if (!peek(TokenKind.PAREN_L)) {
        ArrayBuffer[InputValueDefinition]()
      } else {
        many[InputValueDefinition](TokenKind.PAREN_L, _.parseInputValueDef, TokenKind.PAREN_R)
      }
    }

    /**
     * InputValueDefinition : Name : Type DefaultValue?
     */
    def parseInputValueDef: InputValueDefinition = {
      val start = token.start
      val name = parseName
      expect(TokenKind.COLON)
      val _type = parseType
      val defaultValue = if (skip(TokenKind.EQUALS)) Some(parseConstValue) else None
      InputValueDefinition(name, _type, defaultValue, loc(start))
    }

    /**
     * InterfaceTypeDefinition : interface Name { FieldDefinition+ }
     */
    def parseInterfaceTypeDefinition: InterfaceTypeDefinition = {
      val start = token.start
      expectKeyword("interface")
      val name = parseName
      val fields = any(TokenKind.BRACE_L, _.parseFieldDefinition, TokenKind.BRACE_R)
      InterfaceTypeDefinition(name, fields, loc(start))
    }

    /**
     * UnionTypeDefinition : union Name = UnionMembers
     */
    def parseUnionTypeDefinition: UnionTypeDefinition = {
      val start = token.start
      expectKeyword("union")
      val name = parseName
      expect(TokenKind.EQUALS)
      val types = parseUnionMembers
      UnionTypeDefinition(name, types, loc(start))
    }

    /**
     * UnionMembers :
     *   - NamedType
     *   - UnionMembers | NamedType
     */
    def parseUnionMembers: ArrayBuffer[NamedType] = {
      val members = ArrayBuffer[NamedType]()
      do {
        members.append(parseNamedType)
      } while (skip(TokenKind.PIPE))
      members
    }

    /**
     * ScalarTypeDefinition : scalar Name
     */
    def parseScalarTypeDefinition: ScalarTypeDefinition = {
      val start = token.start
      expectKeyword("scalar")
      val name = parseName
      ScalarTypeDefinition(name, loc(start))
    }

    /**
     * EnumTypeDefinition : enum Name { EnumValueDefinition+ }
     */
    def parseEnumTypeDefinition: EnumTypeDefinition = {
      val start = token.start
      expectKeyword("enum")
      val name = parseName
      val values = many(TokenKind.BRACE_L, _.parseEnumValueDefinition, TokenKind.BRACE_R)
      EnumTypeDefinition(name, values, loc(start))
    }

    /**
     * EnumValueDefinition : EnumValue
     *
     * EnumValue : Name
     */
    def parseEnumValueDefinition: EnumValueDefinition = {
      val start = token.start
      val name = parseName
      EnumValueDefinition(name, loc(start))
    }

    /**
     * InputObjectTypeDefinition : input Name { InputValueDefinition+ }
     */
    def parseInputObjectTypeDefinition: InputObjectTypeDefinition = {
      val start = token.start
      expectKeyword("input")
      val name = parseName
      val fields = any(TokenKind.BRACE_L, _.parseInputValueDef, TokenKind.BRACE_R)
      InputObjectTypeDefinition(name, fields, loc(start))
    }

    /**
     * TypeExtensionDefinition : extend ObjectTypeDefinition
     */
    def parseTypeExtensionDefinition: TypeExtensionDefinition = {
      val start = token.start
      expectKeyword("extend")
      val definition = parseObjectTypeDefinition
      TypeExtensionDefinition(definition, loc(start))
    }
  }


  /**
   * Given a GraphQL source, parses it into a Document.
   * Throws GraphQLError if a syntax error is encountered.
   */
  def parse(source: Source, options: ParseOptions = ParseOptions()): Document = {
    val parser = Parser(source, options)
    parser.parseDocument
  }

  def parse(sourceStr: String): Document = parse(Source(sourceStr))

  /**
   * Given a string containing a GraphQL value, parse the AST for that value.
   * Throws GraphQLError if a syntax error is encountered.
   *
   * This is useful within tools that operate upon GraphQL Values directly and
   * in isolation of complete GraphQL documents.
   */
  def parseValue(source: Source, options: ParseOptions = ParseOptions()): Value = {
    val parser = Parser(source, options)
    parser.parseValueLiteral(false)
  }

  def parseValue(sourceStr: String): Value = parseValue(Source(sourceStr))
}
