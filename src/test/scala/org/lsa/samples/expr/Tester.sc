package org.lsa.samples.expr

object Tester {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import Expr.eval
  
  eval("12")                                      //> res0: Int = 12
  eval("1+2")                                     //> res1: Int = 3
  eval("((2-1)*(2+4))")                           //> res2: Int = 7
}