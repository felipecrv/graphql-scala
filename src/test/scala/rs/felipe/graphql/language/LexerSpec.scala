package rs.felipe.graphql.language

import org.scalatest._
import Matchers._

import rs.felipe.graphql.error.GraphQLError
import rs.felipe.graphql.language.Lexer._

class LexerSpec extends FunSpec {

  private def lexOne(str: String): Token = lex(Source(str))(None)

  private def lexErr(str: String): Token = lex(Source(str))(None)

  describe("Lexer") {
    it("disallows uncommon control characters") {
      (the [GraphQLError] thrownBy { lexErr("\u0007") }).message should
        startWith("Syntax Error GraphQL (1:1) Invalid character \"\\u0007\"");
    }

    it("accepts BOM header") {
      lexOne("\uFEFF foo") shouldBe Token(TokenKind.NAME, 2, 5, Some("foo"))
    }

    it("skips whitespace") {
      // Lex a name
      lexOne("""
               |
               |    foo
               |
               |
               |""".stripMargin) shouldBe Token(TokenKind.NAME, 6, 9, Some("foo"))

      // Lex a name inside comments
      lexOne("""
               |    #comment
               |    foo#comment
               |""".stripMargin) shouldBe Token(TokenKind.NAME, 18, 21, Some("foo"))

      // Lex a token between commas
      lexOne(""",,,foo,,,""") shouldBe Token(TokenKind.NAME, 3, 6, Some("foo"))
    }

    it("errors respect whitespace") {

      (the [GraphQLError] thrownBy {
        lexErr("""
                 |
                 |    ?
                 |
                 |""".stripMargin
        )}).message should equal("""Syntax Error GraphQL (3:5) Unexpected character "?".
                                   |
                                   |2: 
                                   |3:     ?
                                   |       ^
                                   |""".stripMargin)
    }

    it("lexes strings") {
      lexOne("\"simple\"") shouldBe Token(TokenKind.STRING, 0, 8, Some("simple"))

      lexOne("\" white space \"") shouldBe Token(TokenKind.STRING, 0, 15, Some(" white space "))

      lexOne("\"quote \\\"\"") shouldBe Token(TokenKind.STRING, 0, 10, Some("quote \""))

      lexOne("\"escaped \\n\\r\\b\\t\\f\"") shouldBe
        Token(TokenKind.STRING, 0, 20, Some("escaped \n\r\b\t\f"))

      lexOne("\"slashes \\\\ /\"") shouldBe
        Token(TokenKind.STRING, 0, 14, Some("slashes \\ /"))

      lexOne("\"unicode \\u1234\\u5678\\u90AB\\uCDEF\"") shouldBe
        Token(TokenKind.STRING, 0, 34, Some("unicode \u1234\u5678\u90AB\uCDEF"))

    }

    it("lex reports useful string errors") {
      (the [GraphQLError] thrownBy lexErr("\"")).message should
        equal("""Syntax Error GraphQL (1:2) Unterminated string.
                |
                |1: "
                |    ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"no end quote")).message should
        equal("""Syntax Error GraphQL (1:14) Unterminated string.
                |
                |1: "no end quote
                |                ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy {
        lexErr("\"contains unescaped \u0007 control char\"")
      }).message should startWith(
        "Syntax Error GraphQL (1:21) Invalid character within String: \"\\u0007\".")

      (the [GraphQLError] thrownBy {
        lexErr("\"null-byte is not \u0000 end of file\"")
      }).message should startWith(
        "Syntax Error GraphQL (1:19) Invalid character within String: \"\\u0000\".")

      (the [GraphQLError] thrownBy lexErr("\"multi\nline\"")).message should
        equal("""Syntax Error GraphQL (1:7) Unterminated string.
                |
                |1: "multi
                |         ^
                |2: line"
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"multi\rline\"")).message should
        equal("""Syntax Error GraphQL (1:7) Unterminated string.
                |
                |1: "multi
                |         ^
                |2: line"
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"bad \\z esc\"")).message should
        equal("""Syntax Error GraphQL (1:7) Invalid character escape sequence: \z.
                |
                |1: "bad \z esc"
                |         ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"bad \\x esc\"")).message should
        equal("""Syntax Error GraphQL (1:7) Invalid character escape sequence: \x.
                |
                |1: "bad \x esc"
                |         ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"bad \\u1 esc\"")).message should
        equal("""Syntax Error GraphQL (1:7) Invalid character escape sequence: """ + "\\u1 es." + """
                |
                |1: "bad """.stripMargin + "\\u1 esc\"" + """
                |         ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"bad \\u0XX1 esc\"")).message should
        equal("""Syntax Error GraphQL (1:7) Invalid character escape sequence: """ + "\\u0XX1." + """
                |
                |1: "bad """.stripMargin + "\\u0XX1 esc\"" + """
                |         ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"bad \\uXXXX sc\"")).message should
        equal("""Syntax Error GraphQL (1:7) Invalid character escape sequence: """ + "\\uXXXX." + """
                |
                |1: "bad """.stripMargin + "\\uXXXX sc\"" + """
                |         ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"bad \\uFXXX esc\"")).message should
        equal("""Syntax Error GraphQL (1:7) Invalid character escape sequence: """ + "\\uFXXX." + """
                |
                |1: "bad """.stripMargin + "\\uFXXX esc\"" + """
                |         ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\"bad \\uXXXF esc\"")).message should
        equal("""Syntax Error GraphQL (1:7) Invalid character escape sequence: """ + "\\uXXXF." + """
                |
                |1: "bad """.stripMargin + "\\uXXXF esc\"" + """
                |         ^
                |""".stripMargin)
    }

    it("lexes numbers") {

      lexOne("4") shouldBe Token(TokenKind.INT, 0, 1, Some("4"))

      lexOne("4.123") shouldBe Token(TokenKind.FLOAT, 0, 5, Some("4.123"))

      lexOne("-4") shouldBe Token(TokenKind.INT, 0, 2, Some("-4"))

      lexOne("9") shouldBe Token(TokenKind.INT, 0, 1, Some("9"))

      lexOne("0") shouldBe Token(TokenKind.INT, 0, 1, Some("0"))

      lexOne("-4.123") shouldBe Token(TokenKind.FLOAT, 0, 6, Some("-4.123"))

      lexOne("0.123") shouldBe Token(TokenKind.FLOAT, 0, 5, Some("0.123"))

      lexOne("123e4") shouldBe Token(TokenKind.FLOAT, 0, 5, Some("123e4"))

      lexOne("123E4") shouldBe Token(TokenKind.FLOAT, 0, 5, Some("123E4"))

      lexOne("123e-4") shouldBe Token(TokenKind.FLOAT, 0, 6, Some("123e-4"))

      lexOne("123e+4") shouldBe Token(TokenKind.FLOAT, 0, 6, Some("123e+4"))

      lexOne("-1.123e4") shouldBe Token(TokenKind.FLOAT, 0, 8, Some("-1.123e4"))

      lexOne("-1.123E4") shouldBe Token(TokenKind.FLOAT, 0, 8, Some("-1.123E4"))

      lexOne("-1.123e-4") shouldBe Token(TokenKind.FLOAT, 0, 9, Some("-1.123e-4"))

      lexOne("-1.123e+4") shouldBe Token(TokenKind.FLOAT, 0, 9, Some("-1.123e+4"))

      lexOne("-1.123e4567") shouldBe Token(TokenKind.FLOAT, 0, 11, Some("-1.123e4567"))

    }

    it("lex reports useful number errors") {

      (the [GraphQLError] thrownBy lexErr("00")).message should
        equal("""Syntax Error GraphQL (1:2) Invalid number, unexpected digit after 0: "0".
                |
                |1: 00
                |    ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("+1")).message should
        equal("""Syntax Error GraphQL (1:1) Unexpected character "+".
                |
                |1: +1
                |   ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("1.")).message should
        equal("""Syntax Error GraphQL (1:3) Invalid number, expected digit but got: <EOF>.
                |
                |1: 1.
                |     ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr(".123")).message should
        equal("""Syntax Error GraphQL (1:1) Unexpected character ".".
                |
                |1: .123
                |   ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("1.A")).message should
        equal("""Syntax Error GraphQL (1:3) Invalid number, expected digit but got: "A".
                |
                |1: 1.A
                |     ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("-A")).message should
        equal("""Syntax Error GraphQL (1:2) Invalid number, expected digit but got: "A".
                |
                |1: -A
                |    ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("1.0e")).message should
        equal("""Syntax Error GraphQL (1:5) Invalid number, expected digit but got: <EOF>.
                |
                |1: 1.0e
                |       ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("1.0eA")).message should
        equal("""Syntax Error GraphQL (1:5) Invalid number, expected digit but got: "A".
                |
                |1: 1.0eA
                |       ^
                |""".stripMargin)

    }

    it("lexes punctuation") {

      lexOne("!") shouldBe Token(TokenKind.BANG, 0, 1, None)

      lexOne("$") shouldBe Token(TokenKind.DOLLAR, 0, 1, None)

      lexOne("(") shouldBe Token(TokenKind.PAREN_L, 0, 1, None)

      lexOne(")") shouldBe Token(TokenKind.PAREN_R, 0, 1, None)

      lexOne("...") shouldBe Token(TokenKind.SPREAD, 0, 3, None)

      lexOne(":") shouldBe Token(TokenKind.COLON, 0, 1, None)

      lexOne("=") shouldBe Token(TokenKind.EQUALS, 0, 1, None)

      lexOne("@") shouldBe Token(TokenKind.AT, 0, 1, None)

      lexOne("[") shouldBe Token(TokenKind.BRACKET_L, 0, 1, None)

      lexOne("]") shouldBe Token(TokenKind.BRACKET_R, 0, 1, None)

      lexOne("{") shouldBe Token(TokenKind.BRACE_L, 0, 1, None)

      lexOne("|") shouldBe Token(TokenKind.PIPE, 0, 1, None)

      lexOne("}") shouldBe Token(TokenKind.BRACE_R, 0, 1, None)

    }

    it("lex reports useful unknown character error") {

      (the [GraphQLError] thrownBy lexErr("..")).message should
        equal("""Syntax Error GraphQL (1:3) Unexpected character "EOF".
                |
                |1: ..
                |     ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("?")).message should
        equal("""Syntax Error GraphQL (1:1) Unexpected character "?".
                |
                |1: ?
                |   ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\u203B")).message should
        equal("""Syntax Error GraphQL (1:1) Unexpected character """" + "\\u203B" + """".
                |
                |1: \u203B
                |   ^
                |""".stripMargin)

      (the [GraphQLError] thrownBy lexErr("\u200B")).message should
        equal("""Syntax Error GraphQL (1:1) Unexpected character """" + "\\u200B" + """".
                |
                |1: \u200B
                |   ^
                |""".stripMargin)

    }
  }

  it("lex reports useful information for dashes in names") {
    val source = Source("a-b")

    val lexToken = {
      val token = lex(source)
      token
    }

    val firstToken = lexToken(None)
    firstToken shouldBe Token(TokenKind.NAME, 0, 1, Some("a"))

    (the [GraphQLError] thrownBy lexToken(None)).message should
      equal("""Syntax Error GraphQL (1:3) Invalid number, expected digit but got: "b".
              |
              |1: a-b
              |     ^
              |""".stripMargin)
  }
}
