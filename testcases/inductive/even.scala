import leon.annotation._
import leon.lang._
import leon.math._

object Even {

  @isabelle.inductive  
  def even(n: Nat): Boolean = n match {
    case Zero() => true
    case Succ(Succ(n)) => even(n)
  }

  @isabelle.proof(method = """(induct "<var n>", auto)""")
  def even_lemma(n: Nat): Boolean = {
    even(n + n).holds
  }

}
