package boolalgebra.instances

import boolalgebra.BooleanAlgebra

object BooleanBooleanAlgebra extends BooleanAlgebra[Boolean] {
  override def tru: Boolean = true
  override def fls: Boolean = false
  override def not(value: Boolean): Boolean = !value
  override def and(lhs: Boolean, rhs: Boolean): Boolean = lhs && rhs
  override def or(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
}
