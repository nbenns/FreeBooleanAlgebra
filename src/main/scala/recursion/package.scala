import effect.*
import Functor.*
import CoFlatMap.*

package object recursion {
  type FAlgebra[F[_], A] = F[A] => A
  type CoFAlgebra[F[_], A] = A => F[A]

  object FAlgebra {
    given [F[+_]: Functor]: Invariant[FAlgebra[F, _]] = new Invariant[FAlgebra[F, _]] {
      override def imap[A, B](falgA: FAlgebra[F, A])(f: A => B)(g: B => A): FAlgebra[F, B] = fb => f(falgA(fb.map(g)))
    }

    given [F[-_]: CoFlatMap, A, B]: CoKleisli[F, A, B] = new CoKleisli[F, A, B] {
      override def andThenK[C](f: F[A] => B, g: F[B] => C): F[A] => C = fa => g(fa.extend(f))
    }
  }
}
