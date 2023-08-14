package recursion

import effect.Functor.*
import effect.{CoFlatMap, Functor, Monad}
import recursion.Free.cata

enum Free[F[+_], +A] {
  case Pure(value: A)
  case Impure(unFree: F[Free[F, A]])

  def withF[G[+_]]: Free[G, A] = this.asInstanceOf[Free[G, A]]

  def cata[B >: A](falg: FAlgebra[F, B])(using Functor[F]): B = Free.cata(this, falg)

  def run[B](f: A => B)(alg: FAlgebra[F, B])(using Functor[F]): B = Free.cata(this.map(f), alg)
}

object Free {
  def cata[F[+_]: Functor, A](freeOfA: Free[F, A], falg: FAlgebra[F, A]): A = freeOfA match {
    case Pure(value) => value
    case Impure(fofFreeA) => falg(fofFreeA.map(cata(_, falg)))
  }

  def ana[F[+_]: Functor, A](seed: A, calg: CoFAlgebra[F, A]): Free[F, A] =
    Impure(calg(seed).map(ana(_, calg)))

  def hylo[F[+_]: Functor, A, B](seed: A, calg: CoFAlgebra[F, A], falg: FAlgebra[F, B]): B =
    falg(calg(seed).map(hylo(_, calg, falg)))

  given [F[+_]: Functor]: Monad[Free[F, +_]] = new Monad[Free[F, +_]] {
    override def pure[A](a: A): Free[F, A] = Pure[F, A](a)

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa match {
      case Pure(v) => f(v)
      case Impure(m) => Impure(m.map(flatMap(_)(f)))
    }
  }

  given [F[+_]: Functor]: CoFlatMap[Free[F, +_]] = new CoFlatMap[Free[F, +_]] {
    override def duplicate[A](fa: Free[F, A]): Free[F, Free[F, A]] = fa.map(Pure[F, A])
    override def extend[A, B](fa: Free[F, A])(f: Free[F, A] => B): Free[F, B] = duplicate(fa).map(f)
  }
}
