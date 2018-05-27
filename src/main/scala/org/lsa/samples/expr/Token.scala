package org.lsa.samples.expr

trait Token
trait Op extends Token

case class Number(v: Int) extends Token {
  override def toString: String = v.toString
}

case object OBrace extends Token {
  override def toString: String = "("
}

case object CBrace extends Token {
  override def toString: String = ")"
}

case object Plus extends Op {
  override def toString: String = "+"
}

case object Minus extends Op {
  override def toString: String = "-"
}

case object Mult extends Op {
  override def toString: String = "*"
}

case object Div extends Op {
  override def toString: String = "/"
}

case object Empty extends Token {
  override def toString: String = ""
}

case class Error(m: String) extends Token {
  override def toString: String = "Error: " + m
}

trait TokenTree {
  def split: (Token, TokenTree, TokenTree)
}

case object NoNode extends TokenTree {
  def split: (Token, TokenTree, TokenTree) = (Empty, NoNode, NoNode)
  override def toString: String = "_"
}

case class ErrorNode(e: Error) extends TokenTree {
  def split: (Token, TokenTree, TokenTree) = (e, NoNode, NoNode)
  override def toString: String = "!" + e + "!"
}

case class Node(t: Token, l: TokenTree = NoNode, r: TokenTree = NoNode) extends TokenTree {
  def split: (Token, TokenTree, TokenTree) = (t, l, r)
  override def toString: String = (l, r) match {
    case (NoNode, NoNode) => t.toString
    case (NoNode, _)      => "[" + t + ", " + r + "]"
    case (_, NoNode)      => "[" + l + ", " + t + "]"
    case _                => "[" + l + ", " + t + ", " + r + "]"
  }
}

