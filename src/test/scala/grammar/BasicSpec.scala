package grammar

import org.scalatest.FlatSpec

class BasicSpec extends FlatSpec {
  def construct(productions: List[(String, List[String])]): List[Production] = {
    val terminals = productions.map(_._1).toSet
    productions.map(prod =>
      Production(NonTerminal(prod._1), prod._2.map( str =>
        if (terminals.contains(str)) NonTerminal(str)
        else Terminal(str)
      )))
  }

  val productions: List[Production] = construct(List(
    ("E", List("T", "Ep")),
    ("Ep", List("+", "T", "Ep")),
    ("Ep", List("")),
    ("T", List("F", "Tp")),
    ("Tp", List("*", "F", "Tp")),
    ("Tp", List("")),
    ("F", List("(", "E", ")")),
    ("F", List("id")),
  ))
}
