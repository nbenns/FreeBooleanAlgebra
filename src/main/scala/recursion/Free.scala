package recursion

import effect.{Functor, CoFlatMap, Monad}
import Functor._
import scala.language.higherKinds

sealed trait Free[F[_], A]

case class Pure[F[_], A](value: A) extends Free[F, A]
case class Impure[F[_], A](unFree: F[Free[F, A]]) extends Free[F, A]

object Free {
  def cataMorphism[F[_]: Functor, A](ff: Free[F, A], f: FAlgebra[F, A]): A = ff match {
    case Pure(value) => value
    case Impure(free) => f(free.map(cataMorphism(_, f)))
  }

  implicit class FreeOps[F[_]: Functor, A](ff: Free[F, A]) {
    def cata(f: FAlgebra[F, A]): A = cataMorphism[F, A](ff, f)
  }

  implicit def freeMonad[F[_]: Functor]: Monad[Free[F, ?]] = new Monad[Free[F, ?]] {
    override def pure[A](a: A): Free[F, A] = Pure(a)

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa match {
      case Pure(v) => f(v)
      case Impure(m) => Impure(m.map(flatMap(_)(f)))
    }
  }

  implicit def freeCoFlatMap[F[_]: Functor]: CoFlatMap[Free[F, ?]] = new CoFlatMap[Free[F, ?]] {
    override def duplicate[A](fa: Free[F, A]): Free[F, Free[F, A]] = fa.map(Pure[F, A])
    override def extend[A, B](fa: Free[F, A])(f: Free[F, A] => B): Free[F, B] = duplicate(fa).map(f)
  }
}
