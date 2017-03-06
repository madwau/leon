import leon.lang._
import leon.annotation._

object MiniIMP {
  abstract class Aexp
  case class N(n: Int) extends Aexp
  case class V(x: String) extends Aexp
  case class Plus(a1: Aexp, a2: Aexp) extends Aexp
  
  def aval(a: Aexp, s: String => Int): Int = a match {
    case N(n) => n
    case V(x) => s(x)
    case Plus(a1, a2) => aval(a1, s) + aval(a2, s)
  }
  
  def asimp_const(a: Aexp): Aexp = a match {
    case N(n) => N(n)
    case V(x) => V(x)
    case Plus(a1, a2) =>
      (asimp_const(a1), asimp_const(a2)) match {
        case (N(n1), N(n2)) => N(n1 + n2)
        case (b1, b2) => Plus(b1, b2)
      }
  }
  
  sealed abstract class Bexp
  case class Bc(v: Boolean) extends Bexp
  case class Not(b: Bexp) extends Bexp
  case class And(b1: Bexp, b2: Bexp) extends Bexp
  case class Less(a1: Aexp, a2: Aexp) extends Bexp
  
  def bval(a: Bexp, s: String => Int): Boolean = a match {
    case Bc(v) => v
    case Not(b) => !bval(b, s)
    case And(b1, b2) => bval(b1, s) && bval(b2, s)
    case Less(a1, a2) => aval(a1, s) < aval(a2, s)
  }
  
  sealed abstract class Com
  case object Skip extends Com
  case class Assign(x: String, a: Aexp) extends Com
  case class Seq(c1: Com, c2: Com) extends Com
  case class If(b: Bexp, c1: Com, c2: Com) extends Com
  case class While(b: Bexp, c: Com) extends Com

  @isabelle.inductive
  def big_step(c: Com, s1: String => Int, s2: String => Int): Boolean =

    (c, s1, s2) match {

      case (Skip, s1, s2) =>
        s1 == s2

      case (Assign(x, a), s1, s2) =>
        ((y: String) => { if (y == x) aval(a, s1) else s1(y) }) == s2

      case (Seq(c1, c2), s1, s3) =>
        exists { (s2: String => Int) =>
          big_step(c1, s1, s2) && big_step(c2, s2, s3)
        }

      case (If(b, c1, c2), s, t) =>
        if (bval(b, s))
          big_step(c1, s, t)
        else
          big_step(c2, s, t)

      case (While(b, c), s1, s3) =>
        if (bval(b, s1))
          exists { (s2: String => Int) =>
            big_step(c, s1, s2) && big_step(While(b, c), s2, s3)
          }
        else
          s1 == s3
  }
  
  def theorem(x: String, s: String => Int): Boolean = {
    big_step(Assign(x, V(x)), s, s).holds
  }
}

