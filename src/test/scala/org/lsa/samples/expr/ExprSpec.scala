package org.lsa.samples.expr

import org.scalatest._
import org.scalactic.source.Position.apply

class ExprSpec extends FreeSpec {

  "An empty expr should expand to Nil/Empty tree" in {
    val e = new Expr("")
    val tokens: List[Token] = e.tokenList
    assert(e.tokenList == Nil)
    assert(e.tokenTree == NoNode)
    assert(e.eval === 0)
  }
  
  "An integer expr should evaluate as an integer" in {
    val e = new Expr("123")
    assert(e.tokenList == List(Number(123)))
    assert(e.tokenTree == Node(Number(123), NoNode, NoNode))
    assert(e.eval === 123)

    val e2 = new Expr("(123)")
    assert(e2.tokenList == List(OBrace, Number(123), CBrace))
    assert(e2.tokenTree == Node(Number(123), NoNode, NoNode))
    assert(e2.eval === 123)

    val e3 = new Expr("((123))")
    assert(e3.tokenList == List(OBrace, OBrace, Number(123), CBrace, CBrace))
    assert(e3.tokenTree == Node(Number(123), NoNode, NoNode))
    assert(e3.eval === 123)
  }

  "A simple operation expr should evaluate as an operation tree" in {
    val e = new Expr("123+456")
    assert(e.tokenList == List(Number(123), Plus, Number(456)))
    assert(e.tokenTree == Node(Plus, Node(Number(123), NoNode, NoNode), Node(Number(456), NoNode, NoNode)))
    assert(e.eval == 579)
    
    val e2 = new Expr("123 + 456")
    assert(e2.tokenList == List(Number(123), Plus, Number(456)))
    assert(e2.tokenTree == Node(Plus, Node(Number(123), NoNode, NoNode), Node(Number(456), NoNode, NoNode)))
    assert(e2.eval == 579)
    
    val e3 = new Expr("(123 + 456)")
    assert(e3.tokenList == List(OBrace, Number(123), Plus, Number(456), CBrace))
    assert(e3.tokenTree == Node(Plus, Node(Number(123), NoNode, NoNode), Node(Number(456), NoNode, NoNode)))
    assert(e3.eval == 579)
    
    val e4 = new Expr("((123) + (456))")
    assert(e4.tokenList == List(OBrace, OBrace, Number(123), CBrace, Plus, OBrace, Number(456), CBrace, CBrace))
    assert(e4.tokenTree == Node(Plus, Node(Number(123), NoNode, NoNode), Node(Number(456), NoNode, NoNode)))
    assert(e4.eval == 579)
  }

  "A more complex operation expr should evaluate as an operation tree" in {
    val e = new Expr("123+456+789")
    assert(e.tokenList == List(Number(123), Plus, Number(456), Plus, Number(789)))
    assert(e.tokenTree == Node(
        Plus, 
          Node(Number(123), NoNode, NoNode), 
          Node(Plus, Node(Number(456), NoNode, NoNode), Node(Number(789), NoNode, NoNode))))
    assert(e.eval == 1368)
    
    val e2 = new Expr("(123+456)+789")
    assert(e2.tokenList == List(OBrace, Number(123), Plus, Number(456), CBrace, Plus, Number(789)))
    assert(e2.tokenTree == Node(
        Plus, 
          Node(Plus, Node(Number(123), NoNode, NoNode), Node(Number(456), NoNode, NoNode)),
          Node(Number(789), NoNode, NoNode)))
    assert(e2.eval == 1368)
    
    val e3 = new Expr("123+(456+789)")
    assert(e3.tokenList == List(Number(123), Plus, OBrace, Number(456), Plus, Number(789), CBrace))
    assert(e3.tokenTree == Node(
        Plus, 
          Node(Number(123), NoNode, NoNode), 
          Node(Plus, Node(Number(456), NoNode, NoNode), Node(Number(789), NoNode, NoNode))))
    assert(e3.eval == 1368)
  }

}