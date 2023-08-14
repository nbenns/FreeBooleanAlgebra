package effect

trait CoFlatMap[F[_]] {
  def duplicate[A](fa: F[A]): F[F[A]]
  def extend[A, B](fa: F[A])(f: F[A] => B): F[B]
}

object CoFlatMap {
  def apply[F[_]](implicit com: CoFlatMap[F]): CoFlatMap[F] = com

  extension [F[_]: CoFlatMap, A](fa: F[A]) {
    def duplicate: F[F[A]] = CoFlatMap[F].duplicate(fa)
    def extend[B](f: F[A] => B): F[B] = CoFlatMap[F].extend(fa)(f)
  }
}
