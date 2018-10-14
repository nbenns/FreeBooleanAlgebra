package boolalgebra

/*
 * The Trait defines all the objects and functions within Boolean Algebra
 */
trait BooleanAlgebra[A] {
  def tru: A
  def fls: A

  def not(value: A): A

  def and(lhs: A, rhs: A): A
  def or(lhs: A, rhs: A): A
}

object BooleanAlgebra {
  /*
   * Pulls in an instance of a BooleanAlgebra so you don't need to use implicitly
   * or define it as an implicit parameter of a function.
   */
  def apply[A](implicit booleanAlgebra: BooleanAlgebra[A]): BooleanAlgebra[A] = booleanAlgebra

  /*
   * Expose each of the functions of the typeclass as easily accessible functions
   * which will just execute the typeclasses version of those functions
   */
  def tru[A: BooleanAlgebra]: A = BooleanAlgebra[A].tru
  def fls[A: BooleanAlgebra]: A = BooleanAlgebra[A].fls
  def not[A: BooleanAlgebra](value: A): A = BooleanAlgebra[A].not(value)
  def and[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].and(lhs, rhs)
  def or[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].or(lhs, rhs)

  /*
   * Extension methods to provide DSL sugar
   */
  implicit class BooleanAlgebraOps[A: BooleanAlgebra](a: A) {
    def not: A = BooleanAlgebra[A].not(a)
    def and(b: A): A = BooleanAlgebra[A].and(a, b)
    def or(b: A): A = BooleanAlgebra[A].or(a, b)
  }
}