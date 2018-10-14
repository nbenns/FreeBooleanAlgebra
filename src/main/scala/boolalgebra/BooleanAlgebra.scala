package boolalgebra

/*
 * The Trait defines all the objects and functions within Boolean Algebra
 */
trait BooleanAlgebra[A] {
  def tru: A
  def fls: A

  /*
   * Test serves to normalize the result if the implementation uses
   * a range to represent true and false in its algebraic implementation.
   * if the value is considered true then return the tru value
   * if the value is considered false then return the fls value
   */
  def test(a: A): A

  def not(value: A): A =
    if(test(value) == tru) fls
    else tru

  def and(lhs: A, rhs: A): A =
    if ((test(lhs) == tru) && (test(rhs) == tru)) tru
    else fls

  def or(lhs: A, rhs: A): A =
    if (test(lhs) == tru) tru
    else if (test(rhs) == tru) tru
    else fls

  def xor(lhs: A, rhs: A): A =
    if ((test(lhs) == tru) && (test(rhs) == tru)) fls
    else if ((test(lhs) == fls) && (test(rhs) == fls)) fls
    else tru

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
   * which will just execute the typeclasses version of those functions
   */
  def tru[A: BooleanAlgebra]: A = BooleanAlgebra[A].tru
  def fls[A: BooleanAlgebra]: A = BooleanAlgebra[A].fls
  def test[A: BooleanAlgebra](value: A): A = BooleanAlgebra[A].test(value)
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
    def test: A = BooleanAlgebra[A].test(a)
    def unary_! : A = BooleanAlgebra[A].not(a)
    def &(b: A): A = BooleanAlgebra[A].and(a, b)
    def |(b: A): A = BooleanAlgebra[A].or(a, b)
    def ^(b: A): A = BooleanAlgebra[A].xor(a, b)
    def !&(b: A): A = BooleanAlgebra[A].nand(a, b)
    def !|(b: A): A = BooleanAlgebra[A].nor(a, b)
    def !^(b: A): A = BooleanAlgebra[A].nxor(a, b)
  }
}