package searchalgebra

import boolalgebra.{BooleanAlgebra, FreeBooleanAlgebra}
import boolalgebra.instances.BooleanBooleanAlgebra

trait EvaluateSearch[A] {
  def evalSearch(a: A)(search: Search[A]): Boolean
}

object EvaluateSearch {
  def apply[A](implicit evaluate: EvaluateSearch[A]): EvaluateSearch[A] = evaluate

  def evalSearch[A: EvaluateSearch](pred: FreeBooleanAlgebra[Search[A]])(a: A): Boolean = {
    implicit val alg: BooleanAlgebra[Boolean] = BooleanBooleanAlgebra

    FreeBooleanAlgebra.run(pred)(EvaluateSearch[A].evalSearch(a))
  }
}
