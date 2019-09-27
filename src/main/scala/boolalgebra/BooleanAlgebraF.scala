package boolalgebra

import effect.CoFlatMap._
import effect.Functor
import Functor._
import recursion.{FAlgebra, Impure, Free, Pure}
import Free._

sealed trait BooleanAlgebraF[A]

object BooleanAlgebraF {
  type FBAlg[A] = Free[BooleanAlgebraF, A]

  private case class Tru[A]() extends BooleanAlgebraF[A]
  private case class Fls[A]() extends BooleanAlgebraF[A]
  private case class Not[A](value: A) extends BooleanAlgebraF[A]
  private case class And[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]
  private case class Or[A](lhs: A, rhs: A) extends BooleanAlgebraF[A]

  def inject[A](v: A): FBAlg[A] = Pure(v)

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
    override def tru: FBAlg[A] = Impure(Tru())
    override def fls: FBAlg[A] = Impure(Fls())
    override def not(value: FBAlg[A]): FBAlg[A] = Impure(Not(value))
    override def and(lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Impure(And(lhs, rhs))
    override def or(lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Impure(Or(lhs, rhs))
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
          case Fls() => Impure(Tru())
          case Tru() => Impure(Fls())
          case Not(v) => v
          case optv => Impure(Not(Impure(optv)))
        }
      }
    case Or(lhs, rhs) =>
      lhs match {
        case Pure(pureLeft) => rhs match {
          case Pure(pureRight) => Impure(Or(Pure(pureLeft), Pure(pureRight)))
          case Impure(freeRight) => freeRight match {
            case Fls() => Pure(pureLeft)
            case Tru() => Impure(Tru())
            case right => Impure(Or(Pure(pureLeft), Impure(right)))
          }
        }
        case Impure(freeLeft) => freeLeft match {
          case Fls() => rhs
          case Tru() => Impure(Tru())
          case left =>
            rhs match {
              case Pure(pureRight) => Impure(Or(Impure(left), Pure(pureRight)))
              case Impure(freeRight) => freeRight match {
                case Fls() => Impure(left)
                case Tru() => Impure(Tru())
                case right => Impure(Or(Impure(left), Impure(right)))
              }
            }
        }
      }
    case And(lhs, rhs) =>
      lhs match {
        case Pure(pureLeft) => rhs match {
          case Pure(pureRight) => Impure(And(Pure(pureLeft), Pure(pureRight)))
          case Impure(freeRight) => freeRight match {
            case Fls() => Impure(Fls())
            case Tru() => Pure(pureLeft)
            case right => Impure(And(Pure(pureLeft), Impure(right)))
          }
        }
        case Impure(freeLeft) => freeLeft match {
          case Fls() => Impure(Fls())
          case Tru() => rhs
          case left =>
            rhs match {
              case Pure(pureRight) => Impure(And(Impure(left), Pure(pureRight)))
              case Impure(freeRight) => freeRight match {
                case Fls() => Impure(Fls())
                case Tru() => Impure(left)
                case right => Impure(And(Impure(left), Impure(right)))
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
