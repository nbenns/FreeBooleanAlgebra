package recursion

import effect.Functor
import Functor.*

case class Fix[F[+_]](unFix: F[Fix[F]]) {
  def cata[A](falg: FAlgebra[F, A])(using Functor[F]): A = Fix.cata(this, falg)
}

object Fix {
  def cata[F[+_]: Functor, A](ff: Fix[F], f: FAlgebra[F, A]): A = f(ff.unFix.map(cata(_, f)))

  def ana[F[+_] : Functor, A](seed: A, calg: CoFAlgebra[F, A]): Fix[F] =
    Fix(calg(seed).map(ana(_, calg)))

  def hylo[F[+_] : Functor, A, B](seed: A, calg: CoFAlgebra[F, A], falg: FAlgebra[F, B]): B =
    falg(calg(seed).map(hylo(_, calg, falg)))
}
