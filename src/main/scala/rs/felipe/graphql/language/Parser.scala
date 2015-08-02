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
      } else {
        if (options.noSource) {
          Some(Location(start, prevEnd))
        } else {
          Some(Location(start, prevEnd, Some(source)))
        }
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

    def parseDocument: Document = {
      val start = token.start
      val definitions = ArrayBuffer[Definition]()
      do {
        if (peek(TokenKind.BRACE_L)) {
          definitions.append(parseOperationDefinition)
        } else if (peek(TokenKind.NAME)) {
          token.value match {
            case Some("query") | Some("mutation") => definitions.append(parseOperationDefinition)
            case Some("fragment") => definitions.append(parseFragmentDefinition)
            case _ => throw unexpected()
          }
        } else {
          throw unexpected()
        }
      } while (!skip(TokenKind.EOF))

      Document(definitions, loc(start))
    }


    // Implements the parsing rules in the Operations section.

    def parseOperationDefinition: OperationDefinition = {
      val start = token.start
      if (peek(TokenKind.BRACE_L)) {
        OperationDefinition(Query, None, None, None, parseSelectionSet, loc(start))
      } else {
        val operationName = expect(TokenKind.NAME)
        OperationDefinition(
          Operation(operationName.value.getOrElse("")),
          Some(parseName),
          Some(parseVariableDefinitions),
          Some(parseDirectives),
          parseSelectionSet,
          loc(start))
      }
    }

    def parseVariableDefinitions: ArrayBuffer[VariableDefinition] = {
      if (peek(TokenKind.PAREN_L))
        many[VariableDefinition](TokenKind.PAREN_L, _.parseVariableDefinition, TokenKind.PAREN_R)
      else ArrayBuffer[VariableDefinition]()
    }

    def parseVariableDefinition: VariableDefinition = {
      val start = token.start
      val variable = parseVariable
      expect(TokenKind.COLON)
      val _type = parseType
      val defaultValue = if (skip(TokenKind.EQUALS)) Some(parseValue(true)) else None

      VariableDefinition(variable, _type, defaultValue, loc(start))
    }

    def parseVariable: Variable = {
      val start = token.start
      expect(TokenKind.DOLLAR)
      Variable(parseName, loc(start))
    }

    def parseSelectionSet: SelectionSet = {
      val start = token.start
      val parseSelection: ParseFn[Selection] = { parser =>
        if (peek(TokenKind.SPREAD)) {
          val either = parser.parseFragment
          either.left.getOrElse(either.right.get)
        } else {
          parser.parseField
        }
      }
      def selections = many[Selection](TokenKind.BRACE_L, parseSelection, TokenKind.BRACE_R)
      SelectionSet(selections, loc(start))
    }


    /**
     * Corresponds to both Field and Alias in the spec
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

    def parseArguments: ArrayBuffer[Argument] = {
      if (peek(TokenKind.PAREN_L)) many[Argument](TokenKind.PAREN_L, _.parseArgument, TokenKind.PAREN_R)
      else ArrayBuffer[Argument]()
    }

    def parseArgument: Argument = {
      val start = token.start

      val name = parseName
      expect(TokenKind.COLON)
      val value = parseValue(false)
      Argument(name, value, loc(start))
    }


    // Implements the parsing rules in the Fragments section.

    /**
     * Corresponds to both FragmentSpread and InlineFragment in the spec
     */
    def parseFragment: Either[FragmentSpread, InlineFragment] = {
      val start = token.start
      expect(TokenKind.SPREAD)
      if (token.value == "on") {
        advance
        val typeCondition = parseName
        val directives = parseDirectives
        val selectionSet = parseSelectionSet
        Right(InlineFragment(typeCondition, Some(directives), selectionSet, loc(start)))
      } else {
        val name = parseName
        val directives = parseDirectives
        Left(FragmentSpread(name, Some(directives), loc(start)))
      }
    }

    def parseFragmentDefinition: FragmentDefinition = {
      val start = token.start

      expectKeyword("fragment")
      val name = parseName

      expectKeyword("on")
      val typeCondition = parseName

      val directives = parseDirectives
      val selectionSet = parseSelectionSet

      FragmentDefinition(name, typeCondition, Some(directives), selectionSet, loc(start))
    }


    // Implements the parsing rules in the Values section.

    def parseVariableValue: Value = parseValue(false)

    def parseConstValue: Value = parseValue(true)

    def parseValue(isConst: Boolean): Value = {
      val _token = token
      val start = _token.start

      _token.kind match {
        case TokenKind.BRACKET_L => return parseArray(isConst)
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
          advance
          _token.value match {
            case Some("true") => BooleanValue(true, loc(start))
            case Some("false") => BooleanValue(false, loc(start))
            case Some(value) => EnumValue(value, loc(start))
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

    def parseArray(isConst: Boolean): ArrayValue = {
      val start = token.start
      val item: ParseFn[Value] = if (isConst) _.parseConstValue else _.parseVariableValue
      val values = any(TokenKind.BRACKET_L, item, TokenKind.BRACKET_R)
      ArrayValue(values, loc(start))
    }

    def parseObject(isConst: Boolean): ObjectValue = {
      val start = token.start
      expect(TokenKind.BRACE_L)
      val fieldNames = mutable.Map[String, Boolean]()
      val fields = ArrayBuffer[ObjectField]()
      while (!skip(TokenKind.BRACE_R)) {
        fields.append(parseObjectField(isConst, fieldNames))
      }
      ObjectValue(fields, loc(start))
    }

    def parseObjectField(isConst: Boolean, fieldNames: mutable.Map[String, Boolean]): ObjectField = {
      val start = token.start
      val name = parseName
      if (fieldNames.getOrElse(name.value, false)) {
        throw syntaxError(source, start, s"Duplicate input object field ${name.value}.")
      }
      fieldNames(name.value) = true

      expect(TokenKind.COLON)
      val value = parseValue(isConst)

      ObjectField(name, value, loc(start))
    }

    // Implements the parsing rules in the Directives section.

    def parseDirectives: ArrayBuffer[Directive] = {
      val directives = ArrayBuffer[Directive]()
      while (peek(TokenKind.AT)) {
        directives.append(parseDirective)
      }
      directives
    }

    def parseDirective: Directive = {
      val start = token.start
      expect(TokenKind.AT)
      Directive(parseName, Some(parseArguments), loc(start))
    }


    // Implements the parsing rules in the Types section.

    /**
     * Handles the Type: TypeName, ListType, and NonNullType parsing rules.
     */
    def parseType: Type = {
      val start = token.start

      val _type =
        if (skip(TokenKind.BRACKET_L)) {
          val itemType = parseType
          expect(TokenKind.BRACKET_R)
          Right(ListType(itemType, loc(start)))
        } else {
          Left(parseName)
        }

      if (skip(TokenKind.BANG)) {
        NonNullType(_type, loc(start))
      } else {
        _type.left.getOrElse(_type.right.get)
      }
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

  def parse(sourceStr: String): Document = {
    parse(Source(sourceStr))
  }
}
