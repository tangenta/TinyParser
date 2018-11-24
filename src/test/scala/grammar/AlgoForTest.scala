package grammar


class AlgoForTest extends BasicSpec {
  private val productions2 = construct(List(
    ("S", List("A", "x")),
    ("A", List("a")),
    ("B", List("C", "D")),
    ("C", List("c")),
    ("C", List("")),
    ("D", List("d")),
    ("D", List("e")),
  ))

  "First(a) in productions2" should "be { a }" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("a"))) {
      algo.first(Terminal("a"))
    }
  }

  "First(S) in productions2" should "be {a}" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("a"))) {
      algo.first(NonTerminal("S"))
    }
  }

  "First(B) in productions2" should "be {c, d, e}" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("c"), Terminal("d"), Terminal("e"))) {
      algo.first(NonTerminal("B"))
    }
  }
  
  "First(t) of productions" should "be correct" in {
    val algo = AlgoFor(productions)
    val result1 = Set(Terminal("("),Terminal("id"))
    val result2 = Set(Terminal("+"), Terminal(""))
    val result3 = Set(Terminal("*"), Terminal(""))
    assertResult(result1) {
      algo.first(NonTerminal("F"))
    }
    assertResult(result1) {
      algo.first(NonTerminal("T"))
    }
    assertResult(result1) {
      algo.first(NonTerminal("E"))
    }
    assertResult(result2) {
      algo.first(NonTerminal("Ep"))
    }
    assertResult(result3) {
      algo.first(NonTerminal("Tp"))
    }
  }

}
