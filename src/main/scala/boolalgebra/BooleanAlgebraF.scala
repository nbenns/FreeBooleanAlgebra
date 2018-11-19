package boolalgebra

import effect.Functor
import Functor._
import recursion.{FAlgebra, Free}
import Free._

sealed trait BooleanAlgebraF[A]

object BooleanAlgebraF {
  private case class Tru[A]() extends BooleanAlgebraF[A]
  private case class Fls[A]() extends BooleanAlgebraF[A]
  private case class Not[A](value: A) extends BooleanAlgebraF[A]
  private case class And[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class Or[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class XOr[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class NAnd[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class NOr[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class NXOr[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]

  def inject[A](value: A): Free[BooleanAlgebraF, A] = Free(Left(value))

  implicit val fixBAGFunctor: Functor[BooleanAlgebraF] = new Functor[BooleanAlgebraF] {
    override def map[A, B](fa: BooleanAlgebraF[A])(f: A => B): BooleanAlgebraF[B] = fa match {
      case Tru() => Tru()
      case Fls() => Fls()
      case Not(value) => Not(f(value))
      case And(lhs, rhs) => And(f(lhs), f(rhs))
      case Or(lhs, rhs) => Or(f(lhs), f(rhs))
      case XOr(lhs, rhs) => XOr(f(lhs), f(rhs))
      case NAnd(lhs, rhs) => NAnd(f(lhs), f(rhs))
      case NOr(lhs, rhs) => NOr(f(lhs), f(rhs))
      case NXOr(lhs, rhs) => NXOr(f(lhs), f(rhs))
    }
  }

  implicit def boolalg[A]: BooleanAlgebra[Free[BooleanAlgebraF, A]] = new BooleanAlgebra[Free[BooleanAlgebraF, A]] {
    override def tru: Free[BooleanAlgebraF, A] = Free[BooleanAlgebraF, A](Right(Tru()))
    override def fls: Free[BooleanAlgebraF, A] = Free[BooleanAlgebraF, A](Right(Fls()))
    override def not(value: Free[BooleanAlgebraF, A]): Free[BooleanAlgebraF, A] = Free(Right(Not(value)))
    override def and(lhs: Free[BooleanAlgebraF, A], rhs: Free[BooleanAlgebraF, A]): Free[BooleanAlgebraF, A] = Free(Right(And(lhs, rhs)))
    override def or(lhs: Free[BooleanAlgebraF, A], rhs: Free[BooleanAlgebraF, A]): Free[BooleanAlgebraF, A] = Free(Right(Or(lhs, rhs)))
    override def xor(lhs: Free[BooleanAlgebraF, A], rhs: Free[BooleanAlgebraF, A]): Free[BooleanAlgebraF, A] = Free(Right(XOr(lhs, rhs)))
    override def nand(lhs: Free[BooleanAlgebraF, A], rhs: Free[BooleanAlgebraF, A]): Free[BooleanAlgebraF, A] = Free(Right(NAnd(lhs, rhs)))
    override def nor(lhs: Free[BooleanAlgebraF, A], rhs: Free[BooleanAlgebraF, A]): Free[BooleanAlgebraF, A] = Free(Right(NOr(lhs, rhs)))
    override def nxor(lhs: Free[BooleanAlgebraF, A], rhs: Free[BooleanAlgebraF, A]): Free[BooleanAlgebraF, A] = Free(Right(NXOr(lhs, rhs)))
  }

  def interpreter[A: BooleanAlgebra]: FAlgebra[BooleanAlgebraF, A] = {
    case Tru() => BooleanAlgebra[A].tru
    case Fls() => BooleanAlgebra[A].fls
    case Not(value) => BooleanAlgebra[A].not(value)
    case Or(lhs, rhs) => BooleanAlgebra[A].or(lhs, rhs)
    case And(lhs, rhs) => BooleanAlgebra[A].and(lhs, rhs)
    case XOr(lhs, rhs) => BooleanAlgebra[A].xor(lhs, rhs)
    case NAnd(lhs, rhs) => BooleanAlgebra[A].nand(lhs, rhs)
    case NOr(lhs, rhs) => BooleanAlgebra[A].nor(lhs, rhs)
    case NXOr(lhs, rhs) => BooleanAlgebra[A].nxor(lhs, rhs)
  }

  def interpret[A: BooleanAlgebra](ff: Free[BooleanAlgebraF, A]): A = ff.cata(interpreter[A])

  def run[A, B: BooleanAlgebra](ff: Free[BooleanAlgebraF, A])(f: A => B): B = ff.map(f).cata(interpreter[B])
}
