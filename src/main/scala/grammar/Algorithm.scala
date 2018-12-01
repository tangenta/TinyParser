package grammar

import tiny.Productions

import scala.collection.mutable.ListBuffer
import scala.util.Try

object Algorithm {
  private [grammar] def nullable(syms: Set[Symbol]): Boolean = syms.contains(Terminal(""))

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
    case class MNode(nt: NonTerminal, children: ListBuffer[Node]) extends Node
    case class MLeaf(var token: Token) extends Node

    val startingSymbol = MNode(parsingTable.productions.head.head, ListBuffer())

    def helper(nodes: List[Node], tokens: List[Token]): Boolean = {
      if (nodes.isEmpty && tokens.nonEmpty && tokens.head.terminal == Terminal("$")) true
      else nodes.head match {
        case node: MNode =>
          val body = parsingTable.getProd(node.nt)(tokens.head.terminal)    // needs error handling
          if (body.isEmpty) {
            println(node.nt, "\n", tokens.head.terminal)
            println(startingSymbol)
          }
          val prod = body.get
          val newNodes = prod.map {
            case term: Terminal => MLeaf(Token(term))
            case nonTerm: NonTerminal => MNode(nonTerm, ListBuffer())
          }
          newNodes.foreach(node.children += _)
          helper(newNodes ::: nodes.tail, tokens)
        case leaf: MLeaf =>
          if (leaf.token.terminal.str.isEmpty) helper(nodes.tail, tokens)   // drop epsilon
          else {
            assert(leaf.token.terminal == tokens.head.terminal, leaf.token.terminal + ", " + tokens.head.terminal)   // needs error handling
            leaf.token = tokens.head
            helper(nodes.tail, tokens.tail)
          }
      }
    }
    assert(helper(List(startingSymbol), tokens))
    def immutify(node: Node): Node = node match {
      case mNode: MNode => InnerNode(mNode.nt, mNode.children.map(immutify).toList)
      case mLeaf: MLeaf => Leaf(mLeaf.token)
    }

    Try(immutify(startingSymbol))
  }

}

object TestParseTree extends App {
  val source: String =
    """
      |read x;
      |if 0 < x then
      |  fact := 1;
      |  repeat
      |    fact := fact * x;
      |    x := x - 1
      |  until x = 0
      |end;
      |write fact
    """.stripMargin
//  """
//    | v := u * u
//  """.stripMargin
  import Algorithm._
  print(Productions.buildString(buildParseTree(
    buildParsingTable(tiny.Productions.tinyProductions),
    tiny.Scanner.split(source).get).get))
}