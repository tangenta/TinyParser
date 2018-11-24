package grammar

// a set of algorithms for a specific list of productions
case class AlgoFor(productions: List[Production]) {
  def first(symbol: Symbol): Set[Terminal] = {
    symbol match {
      case term: Terminal => Set(term)
      case nonTerm: NonTerminal =>
        val relatedProductions = productions.toSet.filter(_.head == nonTerm)
        relatedProductions.flatMap { prod =>
          val index = prod.body.indexWhere(!first(_).contains(Terminal("")))
          val firstSymbols =
            if (index == -1) prod.body else prod.body.take(index + 1)
          val firstSet = firstSymbols.toSet.flatMap(first)
          if (index != -1)
            firstSet - Terminal("")
          else firstSet
        }
    }
  }
  def follow(symbol: Symbol): Set[Terminal] = {
    def helper(symbo: Symbol, searchedList: Set[Symbol]): Set[Terminal] = {
      if (searchedList.contains(symbo)) Set()
      else {
        val result = for {
          prod: Production <- productions.toSet
          val index = prod.body.indexOf(symbo)
          if index != -1
          i <- if (index == prod.body.size - 1) helper(prod.head, searchedList + symbo)
          else {
            val firstSet = first(prod.body(index + 1))
            if (firstSet.contains(Terminal("")))
              firstSet - Terminal("") ++ helper(prod.head, searchedList + symbo)
            else firstSet
          }
        } yield i
        if (symbo == productions.head.head) result + Terminal("$")
        else result
      }
    }
    helper(symbol, Set())
  }
}
