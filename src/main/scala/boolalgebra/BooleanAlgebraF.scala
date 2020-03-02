package boolalgebra

import effect.CoFlatMap._
import effect.Functor
import effect.Functor._
import recursion.Free._
import recursion.{FAlgebra, Free}

sealed trait BooleanAlgebraF[+A]

object BooleanAlgebraF {
  type FBAlg[+A] = Free[BooleanAlgebraF, A]

  private case object Tru                    extends BooleanAlgebraF[Nothing]
  private case object Fls                    extends BooleanAlgebraF[Nothing]
  private case class  Not[A](value: A)       extends BooleanAlgebraF[A]
  private case class  And[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class  Or[A](lhs: A, rhs: A)  extends BooleanAlgebraF[A]

  def inject[A](v: A): FBAlg[A] = Pure(v)

  private val tru: Free[BooleanAlgebraF, Nothing] = Impure[BooleanAlgebraF, Nothing](Tru)
  private val fls: Free[BooleanAlgebraF, Nothing] = Impure[BooleanAlgebraF, Nothing](Fls)
  private def not[A](value: FBAlg[A]): FBAlg[A] = Impure(Not(value))
  private def and[A](lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Impure(And(lhs, rhs))
  private def or[A](lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Impure(Or(lhs, rhs))

  implicit val fixBAGFunctor: Functor[BooleanAlgebraF] = new Functor[BooleanAlgebraF] {
    override def map[A, B](fa: BooleanAlgebraF[A])(f: A => B): BooleanAlgebraF[B] = fa match {
      case Tru => Tru
      case Fls => Fls
      case Not(value) => Not(f(value))
      case And(lhs, rhs) => And(f(lhs), f(rhs))
      case Or(lhs, rhs) => Or(f(lhs), f(rhs))
    }
  }

  implicit def boolalg[A]: BooleanAlgebra[FBAlg[A]] = new BooleanAlgebra[FBAlg[A]] {
    def tru: FBAlg[A] = BooleanAlgebraF.tru
    def fls: FBAlg[A] = BooleanAlgebraF.fls
    def not(value: FBAlg[A]): FBAlg[A] = BooleanAlgebraF.not(value)
    def and(lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = BooleanAlgebraF.and(lhs, rhs)
    def or(lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = BooleanAlgebraF.or(lhs, rhs)
  }

  def interpreter[A: BooleanAlgebra]: FAlgebra[BooleanAlgebraF, A] = {
    case Tru => BooleanAlgebra[A].tru
    case Fls => BooleanAlgebra[A].fls
    case Not(value) => BooleanAlgebra[A].not(value)
    case Or(lhs, rhs) => BooleanAlgebra[A].or(lhs, rhs)
    case And(lhs, rhs) => BooleanAlgebra[A].and(lhs, rhs)
  }

  private def optimizeNot[A](value: FBAlg[A]): FBAlg[A] = value match {
    case Pure(v) => Pure(v)
    case Impure(f2) => f2 match {
      case Fls => tru
      case Tru => fls
      case Not(v) => v
      case optv => not(Impure(optv))
    }
  }

  private def optimizeOr[A](lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = lhs match {
    case Pure(pureLeft) => rhs match {
      case Pure(pureRight) => or(Pure(pureLeft), Pure(pureRight))
      case Impure(freeRight) => freeRight match {
        case Fls => Pure(pureLeft)
        case Tru => tru
        case right => or(Pure(pureLeft), Impure(right))
      }
    }
    case Impure(freeLeft) => freeLeft match {
      case Fls => rhs
      case Tru => tru
      case left =>
        rhs match {
          case Pure(pureRight) => or(Impure(left), Pure(pureRight))
          case Impure(freeRight) => freeRight match {
            case Fls => Impure(left)
            case Tru => tru
            case right => or(Impure(left), Impure(right))
          }
        }
    }
  }

  private def optimizeAnd[A](lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = (lhs, rhs) match {
    case (Pure(l), Pure(r)) => and(Pure(l), Pure(r))
    case (Pure(_), Impure(Fls)) => fls
    case (Pure(l), Impure(Tru)) => Pure(l)
    case (Pure(l), Impure(other)) => and(Pure(l), Impure(other))

    case (Impure(Fls), Pure(_)) => fls
    case (Impure(Tru), Pure(r)) => Pure(r)
    case (Impure(other), Pure(r)) => and(Impure(other), Pure(r))

    case (Impure(o1), Impure(o2)) => and(Impure(o1), Impure(o2))
  }

  def optimizer[A]: FAlgebra[BooleanAlgebraF, FBAlg[A]] = {
    case Not(value) => optimizeNot(value)
    case Or(lhs, rhs) => optimizeOr(lhs, rhs)
    case And(lhs, rhs) => optimizeAnd(lhs, rhs)
    case other => Impure(other)
  }

  def interpret[A: BooleanAlgebra](ff: FBAlg[A]): A = ff.cata(interpreter[A])

  def run[A, B: BooleanAlgebra](ff: FBAlg[A])(f: A => B): B = ff.map(f).cata(interpreter[B])

  def optimize[A](fbAlg: FBAlg[A]): FBAlg[A] = fbAlg.duplicate.cata(optimizer)
}
