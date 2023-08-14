package boolalgebra

import effect.Functor

/*
 * The free version of the BooleanAlgebra typeclass is used to wrap values
 * that we don't have typeclass instances for.
 * That way we can perform BooleanAlgebra operations on the contained datatype.
 */
enum FreeBooleanAlgebra[+A] {
  /*
   *  Define all of the objects and functions in the BooleanAlgebra typeclass as Data.
   *  The free version is just a data structure describing the operations to perform.
   *  This gives us the ability to manipulate it before we execute it.
   */

  case Tru extends FreeBooleanAlgebra[Nothing]
  case Fls extends FreeBooleanAlgebra[Nothing]
  case Not(value: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  case And(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  case Or(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  case Inject(value: A) extends FreeBooleanAlgebra[A]
}

object FreeBooleanAlgebra {
  // Add Inject to inject values into the free version of the algebra from the outside world
  //  private case class Inject[A](value: A) extends FreeBooleanAlgebra[A]

  def inject[A](a: A): FreeBooleanAlgebra[A] = Inject(a)

  /*
   * The Free version of the algebra needs to have a valid typeclass instance of BooleanAlgebra
   * Here we simply define the implementation to use the data types we have created above.
   */
  given [A]: BooleanAlgebra[FreeBooleanAlgebra[A]] = new BooleanAlgebra[FreeBooleanAlgebra[A]] {
    override def tru: FreeBooleanAlgebra[A] = Tru
    override def fls: FreeBooleanAlgebra[A] = Fls
    override def not(value: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = Not(value)
    override def and(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = And(lhs, rhs)
    override def or(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = Or(lhs, rhs)
  }

  /*
   * We can convert a FreeBooleanAlgebra from type A to type B using a functor
   * This unwraps the structure until it finds a value, then executes the conversion function
   * and rewraps the structure
   */
  given Functor[FreeBooleanAlgebra] = new Functor[FreeBooleanAlgebra] {
    override def map[A, B](fa: FreeBooleanAlgebra[A])(f: A => B): FreeBooleanAlgebra[B] = fa match {
      case Inject(value) => Inject(f(value))
      case Tru => Tru
      case Fls => Fls
      case Not(value) => Not(map(value)(f))
      case Or(freeLHS, freeRHS) => Or(map(freeLHS)(f), map(freeRHS)(f))
      case And(freeLHS, freeRHS) => And(map(freeLHS)(f), map(freeRHS)(f))
    }
  }

  /*
   * In order to get a result from the free version of the algebra we need to interpret it.
   * To execute it, we actually need a valid typeclass instance of BooleanAlgebra.
   * We unwrap the structure and execute the typeclass instance against it.
   */
  def interpret[A: BooleanAlgebra](fa: FreeBooleanAlgebra[A]): A = fa match {
    case Inject(value) => value
    case Tru => BooleanAlgebra[A].tru
    case Fls => BooleanAlgebra[A].fls
    case Not(value) => BooleanAlgebra[A].not(interpret(value))
    case Or(freeLHS, freeRHS) =>
      val lhs = interpret(freeLHS)
      val rhs = interpret(freeRHS)

      BooleanAlgebra[A].or(lhs, rhs)
    case And(freeLHS, freeRHS) =>
      val lhs = interpret(freeLHS)
      val rhs = interpret(freeRHS)

      BooleanAlgebra[A].and(lhs, rhs)
  }

  /*
   * Run does both the conversion and the interpret step and is really just an optimization
   * of the composition of those two functions.
   * It is really heavy to unwrap and rewrap the structure when converting it, just
   * to unwrap it again when we execute it.
   */
  def run[A, B: BooleanAlgebra](fb: FreeBooleanAlgebra[A])(f: A => B): B = fb match {
    case Inject(value) => f(value)
    case Tru => BooleanAlgebra[B].tru
    case Fls => BooleanAlgebra[B].fls
    case Not(value) => BooleanAlgebra[B].not(run(value)(f))
    case Or(freeLHS, freeRHS) =>
      val lhs = run(freeLHS)(f)
      val rhs = run(freeRHS)(f)

      BooleanAlgebra[B].or(lhs, rhs)
    case And(freeLHS, freeRHS) =>
      val lhs = run(freeLHS)(f)
      val rhs = run(freeRHS)(f)

      BooleanAlgebra[B].and(lhs, rhs)
  }

  /*
   * We can optimize the free program using the Laws of Boolean Algebra
   */
  def optimize[A](fb: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = fb match {
    case Inject(v) => Inject(v)
    case Tru => Tru
    case Fls => Fls

    case Not(value) =>
      optimize(value) match {
        case Fls => Tru
        case Tru => Fls
        case Not(v) => v
        case optv => Not(optv)
      }

    case Or(lhs, rhs) =>
      optimize(lhs) match {
        case Fls => optimize(rhs)
        case Tru => Tru
        case left =>
          optimize(rhs) match {
            case Fls => left
            case Tru => Tru
            case right =>
              (left, right) match {
                case (Not(l), Not(r)) => optimize(Not(And(l, r)))
                case (x, Not(y)) if x == y => Tru
                case (Not(x), y) if x == y => Tru
                case _ => Or(left, right)
              }
          }
      }

    case And(lhs, rhs) =>
      optimize(lhs) match {
        case Fls => Fls
        case Tru => optimize(rhs)
        case left =>
          optimize(rhs) match {
            case Fls => Fls
            case Tru => left
            case right =>
              (left, right) match {
                case (Not(l), Not(r)) => optimize(Not(Or(l, r)))
                case (x, Not(y)) if x == y => Fls
                case (Not(x), y) if x == y => Fls
                case _ => And(left, right)
              }
          }
      }
  }
}
