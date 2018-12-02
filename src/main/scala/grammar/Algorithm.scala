package grammar

import tiny.Productions

import scala.collection.mutable.ListBuffer
import scala.util.Try

object Algorithm {
  private [grammar] def nullable(syms: Set[Symbol]): Boolean = syms.contains(Terminal(""))
  private def tr(str: String) = if (str == "$") "EOF" else str

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
        index = prod.body.indexWhere(!first(productions, _).contains(Terminal("")))
        firstSymbols = if (index == -1) prod.body else prod.body.take(index + 1)
        firstSet = firstSymbols.flatMap(first(productions, _)).toSet
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
          index = prod.body.indexOf(symbo)
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

  def parse(parsingTable: ParsingTable, symbols: List[Terminal]): Boolean = {
    val startingSymbol = parsingTable.productions.head.head
    def helper(nt: List[Symbol], rest: List[Terminal]): Boolean = {
      if (rest.isEmpty && nt.isEmpty) true
      else if (rest.isEmpty || nt.isEmpty) false
      else {
        nt.head match {
          case term: Terminal => if (term != rest.head) false else helper(nt.tail, rest.tail)
          case nonTerm: NonTerminal =>
            val body = parsingTable.getProd(nonTerm)(rest.head)
            if (body.isEmpty) false else {
              if (nullable(body.get.toSet)) helper(nt.tail, rest)  // drop epsilon
              else helper(body.get ++ nt.tail, rest)
            }
        }
      }
    }
    helper(List(startingSymbol), symbols)
  }

  def buildParseTree(parsingTable: ParsingTable, tokens: List[Token]): Try[Node] = {
    def throwRTException(description: String, exceptedNT: Symbol, actualT: Terminal): Nothing = {
      throw new RuntimeException(description + "\nexpected: " +
        (first(parsingTable.productions, exceptedNT) - Terminal("")).map(_.str).mkString(" | ") +
        "\nactual: " +  tr(actualT.str)
      )
    }

    def helper(symbol: Symbol, rest: List[Token]): (Node, List[Token]) = symbol match {
      case terminal: Terminal =>
        if (terminal == Terminal("")) (Leaf(Token(terminal)), rest)
        else if (terminal == rest.head.terminal) (Leaf(rest.head), rest.tail)
        else throwRTException("unmatched token", terminal, rest.head.terminal)

      case nonTerminal: NonTerminal =>
        val optionalBody = parsingTable.getProd(nonTerminal)(rest.head.terminal)
        if (optionalBody.isEmpty) throwRTException("empty table", nonTerminal, rest.head.terminal)
        val body = optionalBody.get

        val (children, restTkn) =
          body.foldLeft((List[Node](), rest)) { case ((nodeList, tokenList), subSymbol) =>
            val tmpResult = helper(subSymbol, tokenList)
            (tmpResult._1 :: nodeList, tmpResult._2)
          }
        (InnerNode(nonTerminal, children.reverse), restTkn)
    }
    Try(helper(parsingTable.productions.head.head, tokens)._1)
  }

}
