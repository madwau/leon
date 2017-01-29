import leon.annotation._
import leon.lang._

object TestInductive {

  sealed abstract class IndInt
  case class Suc(value: IndInt) extends IndInt
  case object Zero extends IndInt

  def indInt(n: Int): IndInt = n match {
    case 0 => Zero
    case n => Suc(indInt(n-1))
  }

  @isabelle.inductive
  def even(n: IndInt): Boolean = n match {
    case Zero => true
    case Suc(Zero) => false
    case Suc(Suc(n)) => even(n)
  }

}

