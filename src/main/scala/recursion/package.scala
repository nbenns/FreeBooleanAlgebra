import effect._
import Functor._
import CoFlatMap._

import scala.language.higherKinds

package object recursion {
  type FAlgebra[F[_], A] = F[A] => A

  object FAlgebra {
    implicit def falgFunctor[F[_]: Functor]: Invariant[FAlgebra[F, ?]] = new Invariant[FAlgebra[F, ?]] {
      override def imap[A, B](falgA: FAlgebra[F, A])(f: A => B)(g: B => A): FAlgebra[F, B] = fb => f(falgA(fb.map(g)))
    }

    implicit def falgCompose[F[_]: CoFlatMap, A, B]: CoKleisli[F, A, B] = new CoKleisli[F, A, B] {
      override def andThenK[C](f: F[A] => B, g: F[B] => C): F[A] => C = fa => g(fa.extend(f))
    }
  }
}
