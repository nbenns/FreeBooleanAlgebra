package boolalgebra

/*
 * The free version of the BooleanAlgebra typeclass is used to wrap values
 * that we don't have typeclass instances for.
 * That way we can perform BooleanAlgebra operations on the contained datatype.
 */
sealed trait FreeBooleanAlgebra[+A]

object FreeBooleanAlgebra {
  /*
   *  Define all of the objects and functions in the BooleanAlgebra typeclass as Data.
   *  The free version is just a data structure describing the operations to perform.
   *  This gives us the ability to manipulate it before we execute it.
   */
  private case object Tru extends FreeBooleanAlgebra[Nothing]
  private case object Fls extends FreeBooleanAlgebra[Nothing]
  private case class Test[A](value: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  private case class Not[A](value: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  private case class And[A](lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  private case class Or[A](lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  private case class XOr[A](lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  private case class NAnd[A](lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  private case class NOr[A](lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]
  private case class NXOr[A](lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]) extends FreeBooleanAlgebra[A]

  // Add Inject to inject values into the free version of the algebra from the outside world
  private case class Inject[A](value: A) extends FreeBooleanAlgebra[A]

  def inject[A](a: A): FreeBooleanAlgebra[A] = Inject(a)

  // The Free version of the algebra needs to have a valid typeclass instance of BooleanAlgebra
  implicit def freeBooleanAlgebra[A]: BooleanAlgebra[FreeBooleanAlgebra[A]] = new BooleanAlgebra[FreeBooleanAlgebra[A]] {
    override def tru: FreeBooleanAlgebra[A] = Tru
    override def fls: FreeBooleanAlgebra[A] = Fls
    override def not(value: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = Not(value)
    override def and(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = And(lhs, rhs)
    override def or(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = Or(lhs, rhs)
    override def xor(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = XOr(lhs, rhs)
    override def nand(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = NAnd(lhs, rhs)
    override def nor(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = NOr(lhs, rhs)
    override def nxor(lhs: FreeBooleanAlgebra[A], rhs: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = NXOr(lhs, rhs)
    override def test(a: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = Test(a)
  }

  /*
   * In order to get a result from the free version of the algebra we need to interpret it.
   * To do that we need to convert the type embedded in it to a type that actually HAS a
   * valid typeclass instance of BooleanAlgebra, then we can execute it.
   */
  def run[A, B: BooleanAlgebra](fb: FreeBooleanAlgebra[A])(f: A => B): B = fb match {
    case Inject(value) => f(value)
    case Test(value) => BooleanAlgebra[B].test(run(value)(f))
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
    case XOr(freeLHS, freeRHS) =>
      val lhs = run(freeLHS)(f)
      val rhs = run(freeRHS)(f)

      BooleanAlgebra[B].xor(lhs, rhs)
    case NAnd(freeLHS, freeRHS) =>
      val lhs = run(freeLHS)(f)
      val rhs = run(freeRHS)(f)

      BooleanAlgebra[B].nand(lhs, rhs)
    case NOr(freeLHS, freeRHS) =>
      val lhs = run(freeLHS)(f)
      val rhs = run(freeRHS)(f)

      BooleanAlgebra[B].nor(lhs, rhs)
    case NXOr(freeLHS, freeRHS) =>
      val lhs = run(freeLHS)(f)
      val rhs = run(freeRHS)(f)

      BooleanAlgebra[B].nxor(lhs, rhs)
  }

  /*
   * We can optimize the free program using the Laws of Boolean Algebra
   */
  def optimize[A](fb: FreeBooleanAlgebra[A]): FreeBooleanAlgebra[A] = fb match {
    case Inject(v) => Inject(v)
    case Tru => Tru
    case Fls => Fls
    case Test(v) =>
      optimize(v) match {
        case Tru => Tru
        case Fls => Fls
        case value => Test(value)
      }
    case Not(value) =>
      optimize(value) match {
        case Fls => Tru
        case Tru => Fls
        case Not(v) => v
        case And(lhs, rhs) => optimize(NAnd(lhs, rhs))
        case Or(lhs, rhs) => optimize(NOr(lhs, rhs))
        case XOr(lhs, rhs) => optimize(NXOr(lhs, rhs))
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
            case right => Or(left, right)
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
            case right => And(left, right)
          }
      }

    case XOr(lhs, rhs) =>
      (optimize(lhs), optimize(rhs)) match {
        case (Tru, right) => optimize(Not(right))
        case (Fls, right) => right
        case (left, Tru) => optimize(Not(left))
        case (left, Fls) => left
        case (left, right) => XOr(left, right)
      }

    case NAnd(lhs, rhs) =>
      optimize(lhs) match {
        case Tru => optimize(Not(rhs))
        case Fls => Tru
        case left =>
          optimize(rhs) match {
            case Tru => optimize(Not(left))
            case Fls => left
            case right => NAnd(left, right)
          }
      }

    case NOr(lhs, rhs) =>
      optimize(lhs) match {
        case Tru => Fls
        case Fls => optimize(Not(rhs))
        case left =>
          optimize(rhs) match {
            case Tru => Fls
            case Fls => optimize(Not(left))
            case right => NOr(left, right)
          }
      }

    case NXOr(lhs, rhs) =>
      optimize(lhs) match {
        case Tru => optimize(rhs)
        case Fls => optimize(Not(rhs))
        case left =>
          optimize(rhs) match {
            case Tru => left
            case Fls => optimize(Not(left))
            case right => NXOr(left, right)
          }
      }
  }
}