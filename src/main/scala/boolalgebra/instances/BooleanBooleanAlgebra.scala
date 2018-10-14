package boolalgebra.instances

import boolalgebra.BooleanAlgebra

object BooleanBooleanAlgebra extends BooleanAlgebra[Boolean]{
  override def tru: Boolean = true
  override def fls: Boolean = false
  override def test(a: Boolean): Boolean = a
}
