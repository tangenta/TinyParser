package tiny

import scala.util.Try

object Scanner {
  private val keywords: Set[String] = Set(
    "if", "then", "end", "else", "repeat", "until",
    "read", "write",
  )
  private val symbols: Set[String] = Set(
    ";", ":=", "<", "=", "+", "-", "*", "/", "(", ")"
  )

  private def isBlank(ch: Char): Boolean = " \t\n".contains(ch)
  private def isAlpha(ch: Char): Boolean = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
  private def isDigit(ch: Char): Boolean = ch >= '0' && ch <= '9'

  protected [tiny] def splitIdentifier(str: String): (String, String) = {
    val rest = str.tail
    val result = rest.span(ch => isAlpha(ch) || isDigit(ch) || ch == '_')
    (str.head + result._1, result._2)
  }
  protected [tiny] def splitNumber(str: String): (String, String) = {
    val (firstPart, rest) = str.span(isDigit)
    if (rest.isEmpty || rest.head != '.') (firstPart, rest)
    else {
      val (secondPart, remain) = rest.tail.span(isDigit)
      (firstPart + "." + secondPart, remain)
    }
  }


  def split(str: String): Try[List[String]] = {
    def helper(recognized: List[String], str: String): List[String] = {
      if (str.isEmpty) recognized
      else str.head match {
        case '{' => helper(recognized, str.dropWhile(_ != '}').tail)
        case blank if isBlank(blank) => helper(recognized, str.tail)
        case alpha if isAlpha(alpha) || alpha == '_' =>
          val split = splitIdentifier(str)
          helper(split._1 :: recognized, split._2)
        case digit if isDigit(digit) =>
          val split = splitNumber(str)
          helper(split._1 :: recognized, split._2)
        case symbol if symbols.exists(_.head == symbol) =>
          val matchedSymbol = symbols.find(_.head == symbol).get
          assert(str.startsWith(matchedSymbol))
          val split = str.splitAt(matchedSymbol.length)
          helper(split._1 :: recognized, split._2)
      }
    }
    Try(helper(List(), str).reverse)
  }
}
