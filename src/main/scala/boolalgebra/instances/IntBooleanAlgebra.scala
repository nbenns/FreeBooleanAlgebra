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
  override def test(a: Int): Int =
    if (a >= tru) tru
    else fls
}
