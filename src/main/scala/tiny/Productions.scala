package tiny
import grammar._

import scala.collection.mutable

object Productions {
  val tinyProductions: List[Production] = Algorithm.construct(List(
    "program" -> List("stmt-sequence"),
    "stmt-sequence" -> List("statement", ";statement"),
    ";statement" -> List(";", "statement", ";statement"),
    ";statement" -> List(""),
    "statement" -> List("if-stmt"),
    "statement" -> List("repeat-stmt"),
    "statement" -> List("assign-stmt"),
    "statement" -> List("read-stmt"),
    "statement" -> List("write-stmt"),
    "if-stmt" -> List("if", "exp", "then", "stmt-sequence", "else-stmt-sequence", "end"),
    "else-stmt-sequence" -> List("else", "stmt-sequence"),
    "else-stmt-sequence" -> List(""),
    "repeat-stmt" -> List("repeat", "stmt-sequence", "until", "exp"),
    "assign-stmt" -> List("identifier", ":=", "exp"),
    "read-stmt" -> List("read", "identifier"),
    "write-stmt" -> List("write", "exp"),
    "exp" -> List("simple-exp", "comp-simp-exp"),
    "comp-simp-exp" -> List("comparision-op", "simple-exp"),
    "comp-simp-exp" -> List(""),
    "comparision-op" -> List("<"),
    "comparision-op" -> List("="),
    "simple-exp" -> List("term", "add-op-term"),
    "add-op-term" -> List("addop", "term", "add-op-term"),
    "add-op-term" -> List(""),
    "addop" -> List("+"),
    "addop" -> List("-"),
    "term" -> List("factor", "mul-factor"),
    "mul-factor" -> List("mulop", "factor", "mul-factor"),
    "mul-factor" -> List(""),
    "mulop" -> List("*"),
    "mulop" -> List("/"),
    "factor" -> List("(", "exp", ")"),
    "factor" -> List("number"),
    "factor" -> List("identifier"),
  ))

  private val nullCache = mutable.WeakHashMap[Node, Boolean]()

  def isNull(node: Node): Boolean = {
    nullCache.getOrElseUpdate(node,
      node match {
        case leaf: Leaf => leaf.token.terminal.str.isEmpty
        case iNode: InnerNode => iNode.children.forall(isNull)
      }
    )
  }

  // defined how the node to be printed
  def buildString(root: Node): String = {
    def printOpExp(compExp: InnerNode, firstOperand: Node, indent: Int): String = {
      if (isNull(compExp)) helper(firstOperand, indent)
      else {
        helper(compExp.children.head, indent) + "\n" +
        helper(firstOperand, indent + 1) + "\n" +
        helper(compExp.children.last, indent + 1)
      }
    }
    def printBinOpExp(binOpExp: Node, firstOperand: Node, indent: Int): String = {
      if (isNull(binOpExp)) helper(firstOperand, indent)
      else {
        val subtree = binOpExp.asInstanceOf[InnerNode].children
        helper(subtree.head, indent) + "\n" +
        helper(firstOperand, indent + 1) + "\n" +
        printBinOpExp(subtree.last, subtree(1), indent + 1)
      }
    }

    def helper(subroot: Node, indent: Int): String = {
      if (isNull(subroot)) ""
      else {
        val blank = Array.fill(indent)(' ').mkString
        subroot match {
          case leaf: Leaf => leaf.token.terminal.str match {
            case "identifier" => blank + "id: " + leaf.token.value
            case "number" => blank + "const: " + leaf.token.value
            case op: String if op.length == 1 => blank + "Op: " + op
            case any => blank + any
          }
          case innerNode: InnerNode =>
            val subtree = innerNode.children
            innerNode.symbol.str match {
              case "stmt-sequence" => subtree.map(helper(_, indent)).mkString("\n")
              case ";statement" => helper(subtree(1), indent) + "\n" + helper(subtree(2), indent)
              case "if-stmt" => blank + "If\n" +
                helper(subtree(1), indent + 1) + "\n" +
                blank + "Then\n" +
                helper(subtree(3), indent + 1) + "\n" +
                helper(subtree(4), indent + 1) + "\n" +
                blank + "End"
              case "else-stmt-sequence" => blank + "Else\n" + helper(subtree(1), indent + 1)
              case "repeat-stmt" => blank + "Repeat\n" +
                helper(subtree(1), indent + 1) + "\n" +
                blank + "until\n" + helper(subtree.last, indent + 1)
              case "assign-stmt" => blank + "Assign to: " + helper(subtree.head, 0) + "\n" +
                helper(subtree.last, indent + 1)
              case "read-stmt" => blank + "Read: " + helper(subtree.last, 0)
              case "write-stmt" => blank + "Write:\n" + helper(subtree.last, indent + 1)
              case "exp" => printOpExp(subtree.last.asInstanceOf[InnerNode], subtree.head, indent)
              case "simple-exp" => printBinOpExp(subtree.last, subtree.head, indent)
              case "term" => printBinOpExp(subtree.last, subtree.head, indent)
              case "factor" if subtree.size == 3 => helper(subtree(1), indent)
              case _ => helper(subtree.head, indent)
            }
        }
      }
    }

    nullCache.clear()
    helper(root, 0).replaceAll("\n+", "\n")
  }
}
