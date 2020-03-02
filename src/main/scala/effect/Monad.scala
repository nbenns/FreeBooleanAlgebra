package effect

import scala.language.higherKinds

trait Monad[F[_]] extends Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f andThen pure)
  override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = flatMap(fab)(ab => map(fa)(ab))
  override def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad {
  def apply[F[_]](implicit mon: Monad[F]): Monad[F] = mon

  implicit class MonadOps[F[_]: Monad, A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }
}
