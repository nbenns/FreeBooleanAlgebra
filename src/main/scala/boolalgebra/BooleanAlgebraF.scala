package boolalgebra

import effect.CoFlatMap._
import effect.Functor
import effect.Functor._
import recursion.Free._
import recursion.{FAlgebra, Free}

sealed trait BooleanAlgebraF[+A]

object BooleanAlgebraF {
  type FBAlg[+A] = Free[BooleanAlgebraF, A]

  private case class Tru() extends BooleanAlgebraF[Nothing]
  private case class Fls() extends BooleanAlgebraF[Nothing]
  private case class Not[A](value: A) extends BooleanAlgebraF[A]
  private case class And[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class Or[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]

  def inject[A](v: A): FBAlg[A] = Free.pure(v)

  private val tru: FBAlg[Nothing] = Free.impure[BooleanAlgebraF, Nothing](Tru())
  private val fls: FBAlg[Nothing] = Free.impure[BooleanAlgebraF, Nothing](Fls())
  private def not[A](value: FBAlg[A]): FBAlg[A] = Free.impure(Not(value))
  private def and[A](lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Free.impure(And(lhs, rhs))
  private def or[A](lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Free.impure(Or(lhs, rhs))

  implicit val fixBAGFunctor: Functor[BooleanAlgebraF] = new Functor[BooleanAlgebraF] {
    override def map[A, B](fa: BooleanAlgebraF[A])(f: A => B): BooleanAlgebraF[B] = fa match {
      case Tru() => Tru()
      case Fls() => Fls()
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
    case Tru() => BooleanAlgebra[A].tru
    case Fls() => BooleanAlgebra[A].fls
    case Not(value) => BooleanAlgebra[A].not(value)
    case Or(lhs, rhs) => BooleanAlgebra[A].or(lhs, rhs)
    case And(lhs, rhs) => BooleanAlgebra[A].and(lhs, rhs)
  }

  // Can't figure out how to split this and still get it to work.
  def optimizer[A]: FAlgebra[BooleanAlgebraF, FBAlg[A]] = {
    case Not(value) =>
      value match {
        case Pure(v) => Pure(v)
        case Impure(f2) => f2 match {
          case Fls() => tru
          case Tru() => fls
          case Not(v) => v
          case optv => not(Impure(optv))
        }
      }
    case Or(lhs, rhs) =>
      lhs match {
        case Pure(pureLeft) => rhs match {
          case Pure(pureRight) => or(Pure(pureLeft), Pure(pureRight))
          case Impure(freeRight) => freeRight match {
            case Fls() => Pure(pureLeft)
            case Tru() => tru
            case right => or(Pure(pureLeft), Impure(right))
          }
        }
        case Impure(freeLeft) => freeLeft match {
          case Fls() => rhs
          case Tru() => tru
          case left =>
            rhs match {
              case Pure(pureRight) => or(Impure(left), Pure(pureRight))
              case Impure(freeRight) => freeRight match {
                case Fls() => Impure(left)
                case Tru() => tru
                case right => or(Impure(left), Impure(right))
              }
            }
        }
      }
    case And(lhs, rhs) =>
      lhs match {
        case Pure(pureLeft) => rhs match {
          case Pure(pureRight) => and(Pure(pureLeft), Pure(pureRight))
          case Impure(freeRight) => freeRight match {
            case Fls() => fls
            case Tru() => Pure(pureLeft)
            case right => and(Pure(pureLeft), Impure(right))
          }
        }
        case Impure(freeLeft) => freeLeft match {
          case Fls() => fls
          case Tru() => rhs
          case left =>
            rhs match {
              case Pure(pureRight) => and(Impure(left), Pure(pureRight))
              case Impure(freeRight) => freeRight match {
                case Fls() => fls
                case Tru() => Impure(left)
                case right => and(Impure(left), Impure(right))
              }
            }
        }
      }
    case other => Impure(other)
  }

  def interpret[A: BooleanAlgebra](ff: FBAlg[A]): A = ff.cata(interpreter[A])

  def run[A, B: BooleanAlgebra](ff: FBAlg[A])(f: A => B): B = ff.map(f).cata(interpreter[B])

  def optimize[A](fbAlg: FBAlg[A]): FBAlg[A] = fbAlg.duplicate.cata(optimizer)
}
