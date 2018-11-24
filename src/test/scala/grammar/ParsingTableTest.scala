package grammar

class ParsingTableTest extends BasicSpec {
  val parsingTable = new ParsingTable(productions)
  "A parsing table" should "be able to get production body after storing a production" in {
    val prod = productions.head
    parsingTable.addProd(prod)(Terminal("id"))
    assertResult(prod.body) {
      parsingTable.getProd(prod.head)(Terminal("id")).get
    }
  }

  it should "return true when add a body for the first time" in {
    val prod = productions(5)
    assertResult(true) {
      parsingTable.addProd(prod)(Terminal("*"))
    }
  }

  it should "return false when add more than one body to the same terminal" in {
    val prod1 = productions(1)
    val prod2 = productions(2)
    parsingTable.addProd(prod1)(Terminal("("))
    assertResult(false) {
      parsingTable.addProd(prod2)(Terminal("("))
    }
  }
}
