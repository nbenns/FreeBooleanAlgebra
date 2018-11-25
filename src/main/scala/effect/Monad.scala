package effect

import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad {
  def apply[F[_]](implicit mon: Monad[F]): Monad[F] = mon

  implicit class MonadOps[F[_]: Monad, A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }
}