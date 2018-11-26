package grammar
import scala.collection.{mutable => m}

abstract class Symbol(val str: String)
case class Terminal(override val str: String) extends Symbol(str)
case class NonTerminal(override val str: String) extends Symbol(str)

case class Production(head: NonTerminal, body: List[Symbol])

case class Token(terminal: Terminal, value: String = "")

trait Node
case class InnerNode(symbol: NonTerminal, children: List[Node]) extends Node
case class Leaf(token: Token) extends Node

class ParsingTable(val productions: List[Production]) {
  val terms: Set[NonTerminal] = productions.map(_.head).toSet
  val nonTerms: Set[Symbol] = productions.flatMap(_.body).toSet.filter(_.isInstanceOf[NonTerminal])
  private val table: m.Map[NonTerminal, m.Map[Terminal, List[Symbol]]] = m.Map()

  def addProd(prod: Production)(term: Terminal): Boolean = {
    val oldValueMap = table.getOrElse(prod.head, m.Map())
    if (oldValueMap.get(term).nonEmpty) false
    else {
      table.update(prod.head, oldValueMap.updated(term, prod.body))
      true
    }
  }

  def getProd(nonTerm: NonTerminal)(term: Terminal): Option[List[Symbol]] = {
    table.get(nonTerm).flatMap(_.get(term))
  }
}
