package grammar

import org.scalatest.FlatSpec

class BasicSpec extends FlatSpec {
  val productions: List[Production] = Algorithm.construct(List(
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
