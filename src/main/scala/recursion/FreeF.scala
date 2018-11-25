package recursion

import effect.Monad

import scala.language.higherKinds

sealed trait FreeF[F[_], A]

case class Pure[F[_], A](value: A) extends FreeF[F, A]
case class Free[F[_], A](unFree: F[FreeF[F, A]]) extends FreeF[F, A]

object FreeF {
  import effect.Functor
  import Functor._

  def cataMorphism[F[_]: Functor, A](ff: FreeF[F, A], f: FAlgebra[F, A]): A = ff match {
    case Pure(value) => value
    case Free(free) => f(free.map(cataMorphism(_, f)))
  }

  implicit class FreeOps[F[_]: Functor, A](ff: FreeF[F, A]) {
    def cata(f: FAlgebra[F, A]): A = cataMorphism[F, A](ff, f)
  }

  implicit def freeFunctor[F[_]: Functor]: Functor[FreeF[F, ?]] = new Functor[FreeF[F, ?]] {
    override def map[A, B](fa: FreeF[F, A])(f: A => B): FreeF[F, B] = fa match {
      case Pure(value) => Pure(f(value))
      case Free(free) => Free(free.map(map(_)(f)))
    }
  }

  implicit def freeMonad[F[_]: Functor]: Monad[FreeF[F, ?]] = new Monad[FreeF[F, ?]] {
    override def pure[A](a: A): FreeF[F, A] = Pure(a)

    override def flatMap[A, B](fa: FreeF[F, A])(f: A => FreeF[F, B]): FreeF[F, B] = fa match {
      case Pure(v) => f(v)
      case Free(m) => Free(m.map(flatMap(_)(f)))
    }
  }
}
