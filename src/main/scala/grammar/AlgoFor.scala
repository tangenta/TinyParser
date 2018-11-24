package grammar

// a set of algorithms for a specific list of productions
case class AlgoFor(productions: List[Production]) {
  def first(symbol: Symbol): Set[Terminal] = {
    symbol match {
      case term: Terminal => Set(term)
      case nonTerm: NonTerminal =>
        productions.toSet.filter(_.head == nonTerm).flatMap { prod =>
          first(prod.body.head)
        }
    }
  }
  def follow(symbol: Symbol): Set[Terminal] = {
    Set()
  }
}
