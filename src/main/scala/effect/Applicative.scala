package effect

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  def apply[F[_]](implicit ap: Applicative[F]): Applicative[F] = ap

  implicit class ApplicativeOps[F[_]: Applicative, A, B](fab: F[A => B]) {
    def ap(fa: F[A]): F[B] = Applicative[F].ap(fab)(fa)
  }
}
