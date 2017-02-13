import leon.annotation._
import leon.lang._

object TestInductive {

  sealed abstract class IndInt
  case class Suc(value: IndInt) extends IndInt
  case object Zero extends IndInt

  def add(n: IndInt, m: IndInt): IndInt = n match {
    case Zero => m
    case Suc(n) => add(n, Suc(m))
  }

  @isabelle.inductive
  def even(n: IndInt): Boolean = n match {
    case Zero => true
    case Suc(Suc(n)) => even(n)
  }

  @isabelle.proof(method = """(induct "<var n>", auto)""")
  def even_lemma(n: IndInt): Boolean = {
    even(add(n, n)).holds
  }
}

