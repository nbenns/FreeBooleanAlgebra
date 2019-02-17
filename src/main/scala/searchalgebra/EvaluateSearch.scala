package searchalgebra

import boolalgebra.BooleanAlgebraF.FBAlg
import boolalgebra.instances.BooleanBooleanAlgebra
import boolalgebra.{BooleanAlgebra, BooleanAlgebraF}

trait EvaluateSearch[A] {
  def evalSearch(a: A)(search: Search[A]): Boolean
}

object EvaluateSearch {
  def apply[A](implicit evaluate: EvaluateSearch[A]): EvaluateSearch[A] = evaluate

  def evalSearch[A: EvaluateSearch](pred: FBAlg[Search[A]])(a: A): Boolean = {
    implicit val alg: BooleanAlgebra[Boolean] = BooleanBooleanAlgebra

    BooleanAlgebraF.run(pred)(EvaluateSearch[A].evalSearch(a))
  }
}
