package recursion

import scala.language.higherKinds

case class Free[F[_], A](unFree: Either[A, F[Free[F, A]]])

object Free {
  import effect.Functor
  import Functor._

  def cataMorphism[F[_]: Functor, A](ff: Free[F, A], f: FAlgebra[F, A]): A = ff.unFree match {
    case Left(value) => value
    case Right(free) => f(free.map(cataMorphism(_, f)))
  }

  implicit class FreeOps[F[_]: Functor, A](ff: Free[F, A]) {
    def cata(f: FAlgebra[F, A]): A = cataMorphism[F, A](ff, f)
  }

  implicit def freeFunctor[F[_]: Functor]: Functor[Free[F, ?]] = new Functor[Free[F, ?]] {
    override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa.unFree match {
      case Left(value) => Free(Left(f(value)))
      case Right(free) => Free(Right(free.map(map(_)(f))))
    }
  }
}
