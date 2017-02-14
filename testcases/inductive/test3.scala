import leon.annotation._
import leon.lang._

object TestInductive {

  @isabelle.inductive  
  def even(n: Int): Boolean = n match {
    case 0 => true
    case n => even(n-2) || even(n+2)
  }

  @isabelle.proof(method = """(induct "<var n>", auto)""")
  def even_lemma(n: Int): Boolean = {
    even(n + n).holds
  }

}
