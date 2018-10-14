package boolalgebra.instances

import boolalgebra.BooleanAlgebra

/*
 * We define our BooleanAlgebra over the Integers.
 * Anything > 0 is considered to be True
 * anything <= 0 is considered to be False
 */
object IntBooleanAlgebra extends BooleanAlgebra[Int] {
  override def tru: Int = 1

  override def fls: Int = 0

  override def not(value: Int): Int =
    if (value > 0) fls else tru

  override def and(lhs: Int, rhs: Int): Int =
    if ((lhs > 0) && (rhs > 0)) tru
    else fls

  override def or(lhs: Int, rhs: Int): Int =
    if (lhs > 0) tru
    else if (rhs > 0) tru
    else fls
}
