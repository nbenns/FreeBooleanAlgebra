import scala.language.higherKinds

package object recursion {
  type FAlgebra[F[_], A] = F[A] => A
}
