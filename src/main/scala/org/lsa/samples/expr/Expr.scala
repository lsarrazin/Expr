package org.lsa.samples.expr

class Expr(s: String) {

  def tokenize(e: String): List[Token] = {

    val z: List[(Token, List[Token])] =
      e.toList.map(_ match {
        case '+' => Plus
        case '-' => Minus
        case '*' => Mult
        case '/' => Div
        case '(' => OBrace
        case ')' => CBrace
        case ' ' => Empty
        case '0' => Number(0)
        case '1' => Number(1)
        case '2' => Number(2)
        case '3' => Number(3)
        case '4' => Number(4)
        case '5' => Number(5)
        case '6' => Number(6)
        case '7' => Number(7)
        case '8' => Number(8)
        case '9' => Number(9)
      }).map(t => (t, Nil))

    val v: (Token, List[Token]) =
      z.fold((Empty, Nil))((a: (Token, List[Token]), n: (Token, List[Token])) => {
        (a._1, n._1) match {
          case (Empty, tok)           => (tok, a._2)
          case (Number(l), Number(r)) => (Number(10 * l + r), a._2)
          case (at, tt)               => (tt, a._2 :+ at)
        }
      })

    (v._2 :+ v._1).filter(_ != Empty)
  }

  def compile(tl: List[Token]): TokenTree = {

    def processBraces(node: TokenTree): TokenTree = {

      def splitTree(node: TokenTree, depth: Int): (TokenTree, TokenTree) = {

        val (t: Token, l: TokenTree, r: TokenTree) = node.split

        t match {
          case Empty => (NoNode, NoNode)
          case e: Error => (ErrorNode(e), NoNode)
          case OBrace =>
            if (depth == 0) {
              splitTree(r, depth + 1)
            } else {
              val (sb, sr) = splitTree(r, depth + 1)
              (Node(t, l, sb), sr)
            }
          case CBrace =>
            if (depth == 1) (NoNode, r)
            else {
              val (sb, sr) = splitTree(r, depth - 1)
              (Node(t, l, sb), sr)
            }
          case _ =>
            val (sb, sr) = splitTree(r, depth)
            (Node(t, l, sb), sr)
        }
      }

      val (t: Token, l: TokenTree, r: TokenTree) = node.split
      t match {
        case Empty    => NoNode
        case e: Error => ErrorNode(e)
        case OBrace => {
          val (bb: TokenTree, br: TokenTree) = splitTree(node, 0)
          Node(OBrace, processBraces(bb), processBraces(br))
        }
        case CBrace => NoNode
        case _      => Node(t, processBraces(l), processBraces(r))
      }
    }

    def balanceTree(node: TokenTree): TokenTree = {

      node match {
        case NoNode       => node
        case e: ErrorNode => node
        case n: Node =>
          val br = balanceTree(n.r)
          val (rt: Token, rl: TokenTree, rr: TokenTree) = br.split

          n.t match {
            case Number(_) => rt match {
              case Empty     => node
              case _: Number => br
              case _: Op     => Node(rt, Node(n.t), rr)
              case _         => Node(Error("Spurious value on right of number"))
            }

            case _: Op => rt match {
              case Empty => node
              case _: Op => Node(rt, Node(n.t), br)
              case _     => Node(n.t, n.l, br)
            }

            case OBrace =>
              val blt = balanceTree(n.l)
              rt match {
                case _: Op =>
                  Node(rt, blt, rr)
                case _ =>
                  blt
              }

            case _ => ErrorNode(Error("Unexpected node token " + n.t + " in TokenTree")) 
          }
      }
    }

    if (tl == Nil) NoNode
    else {
      val t: TokenTree = tl.map(t => Node(t)).foldRight[TokenTree](NoNode)((l, n) => Node(l.t, NoNode, n))
      balanceTree(processBraces(t))
    }
  }

  lazy val tokenList: List[Token] = tokenize(s)
  lazy val tokenTree: TokenTree = compile(tokenList)

  def compute(node: TokenTree): Int = {
    node match {
      case NoNode => 0
      case n: Node =>
        val vr = compute(n.r)
        val vl = compute(n.l)
        n.t match {
          case Plus      => vl + vr
          case Minus     => vl - vr
          case Mult      => vl * vr
          case Div       => vl / vr
          case n: Number => n.v
        }
    }
  }

  def eval: Int = compute(tokenTree)

}

object Expr {

  def eval(s: String): Int = new Expr(s).eval
}