import leon.annotation._
import leon.lang._

object TestInductive {

  sealed abstract class IndInt
  case class Suc(value: IndInt) extends IndInt
  case object Zero extends IndInt

  @isabelle.inductive
  def even(n: IndInt): Boolean = n match {
    case Suc(Suc(n)) => even(n)
    case Zero => true
  }

}

