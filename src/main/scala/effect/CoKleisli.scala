package effect

import scala.language.higherKinds

trait CoKleisli[F[_], A, B] {
  def andThenK[C](f: F[A] => B, g: F[B] => C): F[A] => C
}

object CoKleisli {
  implicit class CoKleilsiOps[F[_], A, B](f: F[A] => B)(implicit comp: CoKleisli[F, A, B]) {
    def andThenK[C](g: F[B] => C): F[A] => C = comp.andThenK(f, g)
  }
}
