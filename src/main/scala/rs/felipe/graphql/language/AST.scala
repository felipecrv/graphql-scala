package rs.felipe.graphql.language

object AST {
  type NodeKind = String

  /**
   * Contains a range of UTF-8 character offsets that identify
   * the region of the source from which the AST derived.
   */
  case class Location(start: Int, end: Int, source: Option[Source] = None)

  abstract class Node(val kind: NodeKind) {
    val loc: Option[Location]
  }
}
