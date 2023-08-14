package effect

trait CoKleisli[F[_], A, B] {
  def andThenK[C](f: F[A] => B, g: F[B] => C): F[A] => C
}

object CoKleisli {
  extension [F[_], A, B](f: F[A] => B)(using comp: CoKleisli[F, A, B]) {
    def andThenK[C](g: F[B] => C): F[A] => C = comp.andThenK(f, g)
  }
}
