package leon.test.synthesis

import org.scalatest.FunSuite

import leon.Evaluator
import leon.purescala.Trees._
import leon.purescala.Common._

import leon.synthesis.LinearEquations._
import leon.synthesis.LikelyEq

class LinearEquationsSuite extends FunSuite {

  def i(x: Int) = IntLiteral(x)

  val xId = FreshIdentifier("x")
  val x = Variable(xId)
  val yId = FreshIdentifier("y")
  val y = Variable(yId)

  val aId = FreshIdentifier("a")
  val a = Variable(aId)
  val bId = FreshIdentifier("b")
  val b = Variable(bId)

  def toSum(es: Seq[Expr]) = es.reduceLeft(Plus(_, _))
  
  def checkSameExpr(e1: Expr, e2: Expr, vs: Set[Identifier], defaultMap: Map[Identifier, Expr] = Map()) {
    assert( //this outer assert should not be needed because of the nested one
      LikelyEq(e1, e2, vs, (e1, e2) => {assert(e1 === e2); true}, defaultMap)
    )
  }
  
  test("particularSolution preprocessed") {
    def toExpr(es: Array[Expr]): Expr = {
      val coef: Array[Expr] = es
      val vars: Array[Expr] = Array[Expr](IntLiteral(1)) ++ Array[Expr](x, y)
      es.zip(vars).foldLeft[Expr](IntLiteral(0))( (acc: Expr, p: (Expr, Expr)) => Plus(acc, Times(p._1, p._2)) )
    }

    val t1: Expr = Plus(a, b)
    val e1: Array[Expr] = Array(t1, IntLiteral(4), IntLiteral(22))
    val s1 = particularSolution(Set(aId, bId), Array(xId, yId), e1)
    println(s1)
    println(toExpr(e1))
    checkSameExpr(toExpr(e1), IntLiteral(0), Set(aId, bId), s1)
  }

}