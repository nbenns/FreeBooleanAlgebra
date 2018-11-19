package recursion

import scala.language.higherKinds

case class Fix[F[_]](unFix: F[Fix[F]])

object Fix {
  import effect.Functor
  import Functor._

  def cataMorphism[F[_]: Functor, A](ff: Fix[F], f: FAlgebra[F, A]): A = f(ff.unFix.map(cataMorphism(_, f)))

  implicit class FixOps[F[_]: Functor](ff: Fix[F]) {
    def cata[A](f: FAlgebra[F, A]): A = cataMorphism[F, A](ff, f)
  }
}
