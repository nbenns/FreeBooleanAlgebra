package effect

trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

object Contravariant {
  def apply[F[_]](using cont: Contravariant[F]): Contravariant[F] = cont

  extension [F[_]: Contravariant, A](fa: F[A]) {
    def contramap[B](f: B => A): F[B] = Contravariant[F].contramap(fa)(f)
  }
}
