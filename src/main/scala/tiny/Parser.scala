package tiny

object Parser {
  def parseIntoTree(code: String): String = {
    try {
      val parsingTable = grammar.Algorithm.buildParsingTable(Productions.tinyProductions)
      val tokens = Scanner.split(code).get
      val root = grammar.Algorithm.buildParseTree(parsingTable, tokens).get

      Productions.buildString(root)
    } catch {
      case e: Exception => "Error: " + e.getMessage
    }
  }
}
