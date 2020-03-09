package effect

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(fb)(map(fa)(a => b => (a, b)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map(zip(fa, fb))(f.tupled)
  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]
}

object Applicative {
  def apply[F[_]](implicit ap: Applicative[F]): Applicative[F] = ap

  implicit class ApplicativeOps[F[_]: Applicative, A, B](fab: F[A => B]) {
    def ap: F[A] => F[B] = Applicative[F].ap(_)(fab)
  }

  implicit class LaxMonoidalOps[F[_]: Applicative, A](fa: F[A]) {
    def zip[B]: F[B] => F[(A, B)] = Applicative[F].zip(fa, _)
  }
}
