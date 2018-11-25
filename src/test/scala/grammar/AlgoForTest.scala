package grammar


class AlgoForTest extends BasicSpec {
  private val productions2 = construct(List(
    ("S", List("A", "x", "B")),
    ("A", List("a")),
    ("B", List("C", "D")),
    ("C", List("c")),
    ("C", List("")),
    ("D", List("d")),
    ("D", List("e")),
  ))

  "First(a) in productions2" should "be {a}" in {
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

  "First() of productions" should "be correct" in {
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

  "Follow(S) of productions2" should "be {$}" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("$"))) {
      algo.follow(NonTerminal("S"))
    }
  }

  "Follow(A) of productions2" should "be {x}" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("x"))) {
      algo.follow(NonTerminal("A"))
    }
  }

  "Follow(B) of productions3" should "be {$}" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("$"))) {
      algo.follow(NonTerminal("B"))
    }
  }

  "Follow() or productions" should "be correct" in {
    val algo = AlgoFor(productions)
    val result1 = Set("+", ")", "$").map(Terminal)
    val result2 = Set(")", "$").map(Terminal)
    val result3 = Set("+", "*", ")", "$").map(Terminal)

    assertResult(result1) {algo.follow(NonTerminal("T"))}
    assertResult(result1) {algo.follow(NonTerminal("Tp"))}
    assertResult(result2) {algo.follow(NonTerminal("E"))}
    assertResult(result2) {algo.follow(NonTerminal("Ep"))}
    assertResult(result3) {algo.follow(NonTerminal("F"))}
  }

  "First(AxB) in productions2" should "be {a}" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("a"))) {
      algo.first(List(NonTerminal("A"), Terminal("x"), NonTerminal("B")))
    }
  }
  "First(Cd) in productions2" should "be {c, d}" in {
    val algo = AlgoFor(productions2)
    assertResult(Set(Terminal("c"), Terminal("d"))) {
      algo.first(List(NonTerminal("C"), Terminal("d")))
    }
  }

 "parsing table of productions2" should "be correct" in {
    val algo = AlgoFor(productions2)
    assertResult(List(NonTerminal("A"), Terminal("x"), NonTerminal("B"))) {
      algo.parsingTable.getProd(NonTerminal("S"))(Terminal("a")).get
    }
    assertResult(List(Terminal("a"))) {
      algo.parsingTable.getProd(NonTerminal("A"))(Terminal("a")).get
    }
    assertResult(List(NonTerminal("C"), NonTerminal("D"))) {
      algo.parsingTable.getProd(NonTerminal("B"))(Terminal("c")).get
    }
    assertResult(List(NonTerminal("C"), NonTerminal("D"))) {
      algo.parsingTable.getProd(NonTerminal("B"))(Terminal("d")).get
    }
    assertResult(List(NonTerminal("C"), NonTerminal("D"))) {
      algo.parsingTable.getProd(NonTerminal("B"))(Terminal("e")).get
    }
    assertResult(List(Terminal("c"))) {
      algo.parsingTable.getProd(NonTerminal("C"))(Terminal("c")).get
    }
    assertResult(List(Terminal(""))) {
      algo.parsingTable.getProd(NonTerminal("C"))(Terminal("d")).get
    }
    assertResult(List(Terminal(""))) {
      algo.parsingTable.getProd(NonTerminal("C"))(Terminal("e")).get
    }
    assertResult(List(Terminal("d"))) {
      algo.parsingTable.getProd(NonTerminal("D"))(Terminal("d")).get
    }
    assertResult(List(Terminal("e"))) {
      algo.parsingTable.getProd(NonTerminal("D"))(Terminal("e")).get
    }
  }
}
