package boolalgebra

import effect.Functor
import Functor._
import recursion.{Free, FAlgebra, FreeF, Pure}
import FreeF._

sealed trait BooleanAlgebraF[A]

object BooleanAlgebraF {
  type FBAlg[A] = FreeF[BooleanAlgebraF, A]

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
    override def tru: FBAlg[A] = Free(Tru())
    override def fls: FBAlg[A] = Free(Fls())
    override def not(value: FBAlg[A]): FBAlg[A] = Free(Not(value))
    override def and(lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Free(And(lhs, rhs))
    override def or(lhs: FBAlg[A], rhs: FBAlg[A]): FBAlg[A] = Free(Or(lhs, rhs))
  }

  def interpreter[A: BooleanAlgebra]: FAlgebra[BooleanAlgebraF, A] = {
    case Tru() => BooleanAlgebra[A].tru
    case Fls() => BooleanAlgebra[A].fls
    case Not(value) => BooleanAlgebra[A].not(value)
    case Or(lhs, rhs) => BooleanAlgebra[A].or(lhs, rhs)
    case And(lhs, rhs) => BooleanAlgebra[A].and(lhs, rhs)
  }

  def optimizer[A]: FAlgebra[BooleanAlgebraF, FBAlg[A]] = {
    case Tru() => Free(Tru())
    case Fls() => Free(Fls())

    case Not(value) =>
      value match {
        case Pure(v) => Pure(v)
        case Free(f2) => f2 match {
          case Fls() => Free(Tru())
          case Tru() => Free(Fls())
          case Not(v) => v
          case optv => Free(Not(Free(optv)))
        }
      }

    case Or(lhs, rhs) =>
      lhs match {
        case Pure(pureLeft) => rhs match {
          case Pure(pureRight) => Free(Or(Pure(pureLeft), Pure(pureRight)))
          case Free(freeRight) => freeRight match {
            case Fls() => Pure(pureLeft)
            case Tru() => Free(Tru())
            case right => Free(Or(Pure(pureLeft), Free(right)))
          }
        }
        case Free(freeLeft) => freeLeft match {
          case Fls() => rhs
          case Tru() => Free(Tru())
          case left =>
            rhs match {
              case Pure(pureRight) => Free(Or(Free(left), Pure(pureRight)))
              case Free(freeRight) => freeRight match {
                case Fls() => Free(left)
                case Tru() => Free(Tru())
                case right => Free(Or(Free(left), Free(right)))
              }
            }
        }
      }

    case And(lhs, rhs) =>
      lhs match {
        case Pure(pureLeft) => rhs match {
          case Pure(pureRight) => Free(And(Pure(pureLeft), Pure(pureRight)))
          case Free(freeRight) => freeRight match {
            case Fls() => Free(Fls())
            case Tru() => Pure(pureLeft)
            case right => Free(And(Pure(pureLeft), Free(right)))
          }
        }
        case Free(freeLeft) => freeLeft match {
          case Fls() => Free(Fls())
          case Tru() => rhs
          case left =>
            rhs match {
              case Pure(pureRight) => Free(And(Free(left), Pure(pureRight)))
              case Free(freeRight) => freeRight match {
                case Fls() => Free(Fls())
                case Tru() => Free(left)
                case right => Free(And(Free(left), Free(right)))
              }
            }
        }
      }
  }

  def interpret[A: BooleanAlgebra](ff: FBAlg[A]): A = ff.cata(interpreter[A])

  def run[A, B: BooleanAlgebra](ff: FBAlg[A])(f: A => B): B = ff.map(f).cata(interpreter[B])

  def optimize[A](fbAlg: FBAlg[A]): FBAlg[A] = fbAlg.map(a => inject(a)).cata(optimizer)
}
