package grammar

object Algorithm {
  private [grammar] def nullable(syms: Set[Terminal]): Boolean = syms.contains(Terminal(""))

  def construct(productions: List[(String, List[String])]): List[Production] = {
    val terminals = productions.map(_._1).toSet
    productions.map(prod =>
      Production(NonTerminal(prod._1), prod._2.map( str =>
        if (terminals.contains(str)) NonTerminal(str)
        else Terminal(str)
      )))
  }

  def first(productions: List[Production], symbol: Symbol): Set[Terminal] = symbol match {
    case term: Terminal => Set(term)
    case nonTerm: NonTerminal =>
      val relatedProductions = productions.toSet.filter(_.head == nonTerm)
      for {
        prod <- relatedProductions
        val index = prod.body.indexWhere(!first(productions, _).contains(Terminal("")))
        val firstSymbols = if (index == -1) prod.body else prod.body.take(index + 1)
        val firstSet = firstSymbols.flatMap(first(productions, _)).toSet
        i <- if (index != -1) firstSet - Terminal("") else firstSet
      } yield i
  }

  def first(productions: List[Production], symbols: List[Symbol]): Set[Terminal] = {
    val index = symbols.indexWhere(!first(productions, _).contains(Terminal("")))
    if (index == -1) symbols.flatMap(first(productions, _)).toSet
    else symbols.take(index + 1).flatMap(first(productions, _)).toSet - Terminal("")
  }

  def follow(productions: List[Production], symbol: Symbol): Set[Terminal] = {
    def helper(symbo: Symbol, searchedList: Set[Symbol]): Set[Terminal] = {
      if (searchedList.contains(symbo)) Set()
      else {
        val result = for {
          prod <- productions
          val index = prod.body.indexOf(symbo)
          if index != -1
          i <- if (index == prod.body.size - 1) helper(prod.head, searchedList + symbo)
          else {
            val firstSet = first(productions, prod.body(index + 1))
            if (firstSet.contains(Terminal("")))
              firstSet - Terminal("") ++ helper(prod.head, searchedList + symbo)
            else firstSet
          }
        } yield i
        if (symbo == productions.head.head) result.toSet + Terminal("$")
        else result.toSet
      }
    }
    helper(symbol, Set())
  }

  def buildParsingTable(productions: List[Production]): ParsingTable = {
    val pt = new ParsingTable(productions)
    for (prod <- productions) {
      val firstSet = first(productions, prod.body)
      val terms = if (firstSet.contains(Terminal(""))) {
        firstSet - Terminal("") ++ follow(productions, prod.head)
      } else {
        firstSet
      }
      terms.foreach(pt.addProd(prod)(_))
    }
    pt
  }
}