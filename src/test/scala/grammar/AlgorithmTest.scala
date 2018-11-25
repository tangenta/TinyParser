package grammar


class AlgorithmTest extends BasicSpec {
  private val productions2 = Algorithm.construct(List(
    ("S", List("A", "x", "B")),
    ("A", List("a")),
    ("B", List("C", "D")),
    ("C", List("c")),
    ("C", List("")),
    ("D", List("d")),
    ("D", List("e")),
  ))

  private def testP2First(symbol: Symbol): Set[Terminal] = {
    Algorithm.first(productions2, symbol)
  }

  private def testPFirst(symbol: Symbol): Set[Terminal] = {
    Algorithm.first(productions, symbol)
  }

  private def testP2Follow(symbol: Symbol): Set[Terminal] = {
    Algorithm.follow(productions2, symbol)
  }

  private def testPFollow(symbol: Symbol): Set[Terminal] = {
    Algorithm.follow(productions, symbol)
  }

  "First(a) in productions2" should "be {a}" in {
    assertResult(Set(Terminal("a"))) {
      testP2First(Terminal("a"))
    }
  }

  "First(S) in productions2" should "be {a}" in {
    assertResult(Set(Terminal("a"))) {
      testP2First(NonTerminal("S"))
    }
  }

  "First(B) in productions2" should "be {c, d, e}" in {
    assertResult(Set(Terminal("c"), Terminal("d"), Terminal("e"))) {
      testP2First(NonTerminal("B"))
    }
  }

  "First(AxB) in productions2" should "be {a}" in {
    assertResult(Set(Terminal("a"))) {
      Algorithm.first(productions2, List(NonTerminal("A"), Terminal("x"), NonTerminal("B")))
    }
  }
  "First(Cd) in productions2" should "be {c, d}" in {
    assertResult(Set(Terminal("c"), Terminal("d"))) {
      Algorithm.first(productions2, List(NonTerminal("C"), Terminal("d")))
    }
  }

  "First() of productions" should "be correct" in {
    val result1 = Set(Terminal("("), Terminal("id"))
    val result2 = Set(Terminal("+"), Terminal(""))
    val result3 = Set(Terminal("*"), Terminal(""))
    assertResult(result1) {
      testPFirst(NonTerminal("F"))
    }
    assertResult(result1) {
      testPFirst(NonTerminal("T"))
    }
    assertResult(result1) {
      testPFirst(NonTerminal("E"))
    }
    assertResult(result2) {
      testPFirst(NonTerminal("Ep"))
    }
    assertResult(result3) {
      testPFirst(NonTerminal("Tp"))
    }
  }

  "Follow(S) of productions2" should "be {$}" in {
    assertResult(Set(Terminal("$"))) {
      Algorithm.follow(productions2, NonTerminal("S"))
    }
  }

  "Follow(A) of productions2" should "be {x}" in {
    assertResult(Set(Terminal("x"))) {
      testP2Follow(NonTerminal("A"))
    }
  }

  "Follow(B) of productions2" should "be {$}" in {
    assertResult(Set(Terminal("$"))) {
      testP2Follow(NonTerminal("B"))
    }
  }

  "Follow() or productions" should "be correct" in {
    val result1 = Set("+", ")", "$").map(Terminal)
    val result2 = Set(")", "$").map(Terminal)
    val result3 = Set("+", "*", ")", "$").map(Terminal)

    assertResult(result1) {
      testPFollow(NonTerminal("T"))
    }
    assertResult(result1) {
      testPFollow(NonTerminal("Tp"))
    }
    assertResult(result2) {
      testPFollow(NonTerminal("E"))
    }
    assertResult(result2) {
      testPFollow(NonTerminal("Ep"))
    }
    assertResult(result3) {
      testPFollow(NonTerminal("F"))
    }
  }

  "parsing table of productions2" should "be correct" in {
    def wrap(str: String): List[Symbol] = {
      str.map(ch =>
        if (ch.isUpper) NonTerminal(ch.toString)
        else Terminal(ch.toString)
      ).toList
    }

    def place(nonTerm: String, term: String): List[Symbol] = {
      Algorithm.buildParsingTable(productions2).getProd(NonTerminal(nonTerm))(Terminal(term)).get
    }

    assertResult(wrap("AxB")) {
      place("S", "a")
    }
    assertResult(wrap("a")) {
      place("A", "a")
    }
    assertResult(wrap("CD")) {
      place("B", "c")
    }
    assertResult(wrap("CD")) {
      place("B", "d")
    }
    assertResult(wrap("CD")) {
      place("B", "e")
    }
    assertResult(wrap("c")) {
      place("C", "c")
    }
    assertResult(List(Terminal(""))) {
      place("C", "d")
    }
    assertResult(List(Terminal(""))) {
      place("C", "e")
    }
    assertResult(List(Terminal("d"))) {
      place("D", "d")
    }
    assertResult(List(Terminal("e"))) {
      place("D", "e")
    }
  }

  "parse for productions2" should "be correct" in {
    import Algorithm._
    val parseTable = buildParsingTable(productions2)
    assert(parse(parseTable,
      "axe".map(e => Terminal(e.toString)).toList))
    assert(parse(parseTable,
      "axcd".map(e => Terminal(e.toString)).toList))
    assert(!parse(parseTable,
      "xx".map(e => Terminal(e.toString)).toList))
  }
}
