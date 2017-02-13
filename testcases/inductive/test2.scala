import leon.annotation._
import leon.lang._

object TestInductive {

  @isabelle.inductive  
  def even(n: BigInt): Boolean = n match {
    case BigInt(0) => true
    case n => even(n-2)
  }

  @isabelle.proof(method = """(induct "<var n>", auto)""")
  def even_lemma(n: BigInt): Boolean = {
    even(n + n).holds
  }

}
