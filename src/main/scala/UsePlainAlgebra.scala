import boolalgebra.BooleanAlgebra
import boolalgebra.BooleanAlgebra.*
import boolalgebra.instances.IntBooleanAlgebra

import scala.language.postfixOps

object UsePlainAlgebra extends App {
  implicit val intBoolAlg: BooleanAlgebra[Int] = IntBooleanAlgebra

  /*
   * We can define it using the helper functions
   */
  val res0: Int = or(and(5, not(fls)), not(tru))
  println(s"Result of our BooleanAlgebra of Int using Functions: $res0")

  /*
   * We can also make use of extension methods
   * Giving us a nicer DSL
   */
  val res1: Int = 5 & !fls | !tru

  println(s"Result of our BooleanAlgebra of Int program using Extension Methods: $res1")
}
