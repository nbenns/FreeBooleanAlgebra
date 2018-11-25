package boolalgebra.instances

import boolalgebra.BooleanAlgebra

/*
 * We define our BooleanAlgebra over the Integers.
 * Anything >= 1 is considered to be True
 * anything < 1 is considered to be False
 */
object IntBooleanAlgebra extends BooleanAlgebra[Int] {
  private def normalize(value: Int): Int =
    if (value > 0) tru
    else fls

  override def tru: Int = 1
  override def fls: Int = 0

  override def not(value: Int): Int =
    if (normalize(value) == tru) fls
    else tru

  override def and(lhs: Int, rhs: Int): Int =
    if ((normalize(lhs) + normalize(rhs)) == 2) tru
    else fls

  override def or(lhs: Int, rhs: Int): Int =
    if ((normalize(lhs) + normalize(rhs)) > 0) tru
    else fls
}
