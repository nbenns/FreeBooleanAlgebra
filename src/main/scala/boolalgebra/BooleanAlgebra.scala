package boolalgebra

/*
 * The Trait defines all the objects and functions within our Boolean Algebra
 */
trait BooleanAlgebra[A] {
  def tru: A
  def fls: A

  def not(value: A): A

  def and(lhs: A, rhs: A): A

  def or(lhs: A, rhs: A): A

  def xor(lhs: A, rhs: A): A

  def nand(lhs: A, rhs: A): A = not(and(lhs, rhs))
  def nor(lhs: A, rhs: A): A = not(or(lhs, rhs))
  def nxor(lhs: A, rhs: A): A = not(xor(lhs, rhs))
}

object BooleanAlgebra {
  /*
   * Pulls in an instance of a BooleanAlgebra so you don't need to use implicitly
   * or define it as an implicit parameter of a function.
   */
  def apply[A](implicit booleanAlgebra: BooleanAlgebra[A]): BooleanAlgebra[A] = booleanAlgebra

  /*
   * Expose each of the functions of the typeclass as easily accessible functions
   * which will just execute the typeclasses' version of those functions
   */
  def tru[A: BooleanAlgebra]: A = BooleanAlgebra[A].tru
  def fls[A: BooleanAlgebra]: A = BooleanAlgebra[A].fls
  def not[A: BooleanAlgebra](value: A): A = BooleanAlgebra[A].not(value)
  def and[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].and(lhs, rhs)
  def or[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].or(lhs, rhs)
  def xor[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].xor(lhs, rhs)
  def nand[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].nand(lhs, rhs)
  def nor[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].nor(lhs, rhs)
  def nxor[A: BooleanAlgebra](lhs: A, rhs: A): A = BooleanAlgebra[A].nxor(lhs, rhs)

  /*
   * Extension methods to provide DSL sugar
   */
  implicit class BooleanAlgebraOps[A: BooleanAlgebra](a: A) {
    def unary_! : A = BooleanAlgebra[A].not(a)
    def &(b: A): A = BooleanAlgebra[A].and(a, b)
    def |(b: A): A = BooleanAlgebra[A].or(a, b)
    def ^(b: A): A = BooleanAlgebra[A].xor(a, b)
    def !&(b: A): A = BooleanAlgebra[A].nand(a, b)
    def !|(b: A): A = BooleanAlgebra[A].nor(a, b)
    def !^(b: A): A = BooleanAlgebra[A].nxor(a, b)
  }
}