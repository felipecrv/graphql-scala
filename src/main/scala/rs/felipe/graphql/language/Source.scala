package rs.felipe.graphql.language

case class Source(body: String, name: String = "GraphQL") {

  /**
   * Takes a UTF-8 character offset, and returns the corresponding
   * line and column as a SourceLocation.
   */
  def getLocation(position: Int): SourceLocation = {
    val linePattern = """\r\n|[\n\r\u2028\u2029]""".r.pattern
    val matcher = linePattern.matcher(body)

    def find(line: Int, column: Int): SourceLocation = {
      if (matcher.find && matcher.start < position) {
        find(line + 1, position + 1 - matcher.end)
      } else {
        SourceLocation(line, column)
      }
    }

    find(1, position + 1)
  }

  lazy val lines = body.split("""\r\n|[\n\r\u2028\u2029]""").toArray
}
