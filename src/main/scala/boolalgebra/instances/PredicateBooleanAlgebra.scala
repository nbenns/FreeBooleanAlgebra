package boolalgebra.instances

import boolalgebra.BooleanAlgebra

import java.util.function.Predicate

final class PredicateBooleanAlgebra[A] extends BooleanAlgebra[Predicate[A]] {
  override def tru: Predicate[A] = _ => true

  override def fls: Predicate[A] = _ => false

  override def not(value: Predicate[A]): Predicate[A] = Predicate.not(value)

  override def and(lhs: Predicate[A], rhs: Predicate[A]): Predicate[A] = lhs.and(rhs)

  override def or(lhs: Predicate[A], rhs: Predicate[A]): Predicate[A] = lhs.or(rhs)
}
