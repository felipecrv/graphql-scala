package rs.felipe.graphql.error

import scala.collection.mutable.ArrayBuffer

import rs.felipe.graphql.language.AST._
import rs.felipe.graphql.language.Source
import rs.felipe.graphql.language.SourceLocation

case class GraphQLError(
    message: String,
    nodes: Option[ArrayBuffer[Node]] = None,
    private val stack: Option[String] = None) extends Exception(message) {

  def getStack = stack.getOrElse(message)

  var positions: Option[ArrayBuffer[Int]] = nodes match {
    case Some(nodeArray) => {
      val positionsArray: ArrayBuffer[Int] = nodeArray.map(_.loc.map(_.start).getOrElse(0))
      if (positionsArray.exists(_ > 0)) {
        Some(positionsArray)
      } else {
        None
      }
    }
    case None => None
  }

  private val loc: Option[Location] = nodes.map(_.headOption.map(_.loc).flatten).flatten

  var source: Option[Source] = loc.map(_.source).flatten

  var locations: Option[ArrayBuffer[SourceLocation]] = {
    (positions, source) match {
      case (Some(positionsArray), Some(src)) => Some(positionsArray.map(src.getLocation(_)))
      case _ => None
    }
  }
}

object GraphQLError {
  /**
   * Produces a GraphQLError representing a syntax error, containing useful
   * descriptive information about the syntax error's position in the source.
   */
  def syntaxError(source: Source, position: Int, description: String): GraphQLError = {
    val location = source.getLocation(position)
    val error = GraphQLError(
      s"Syntax Error ${source.name} (${location.line}:${location.column}) ${description}\n\n" +
      highlightSourceAtLocation(source, location))
    error.positions = Some(ArrayBuffer(position))
    error.locations = Some(ArrayBuffer(location))
    error.source = Some(source)
    error
  }

  /**
   * Render a helpful description of the location of the error in the GraphQL
   * Source document.
   */
  private def highlightSourceAtLocation(source: Source, location: SourceLocation): String = {
    val line = location.line
    val prevLineNum = (line - 1).toString
    val lineNum = line.toString
    val nextLineNum = (line + 1).toString
    val padLen = nextLineNum.length
    val lines = source.lines
    val text: String =
      (
        if (line > 1) lpad(padLen, prevLineNum) + ": " + lines(line - 2) + "\n"
        else ""
      ) +
      lpad(padLen, lineNum) + ": " + lines(line - 1) + "\n" +
      (" " * (1 + padLen + location.column)) + "^\n" +
      (
        if (line < lines.length) lpad(padLen, nextLineNum) + ": " + lines(line) + "\n"
        else ""
      )

    text
  }

  private def lpad(len: Int, str: String): String = " " * (len - str.length) + str
}
