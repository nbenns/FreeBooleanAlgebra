import boolalgebra.BooleanAlgebra._
import boolalgebra.FreeBooleanAlgebra
import boolalgebra.FreeBooleanAlgebra._
import boolalgebra.instances.IntBooleanAlgebra

import scala.language.postfixOps

object UseFreeAlgebra extends App {
  /*
   * We create a BooleanAlgebra over Strings by injecting it into the FreeBooleanAlgebra
   * Even though we don't have a BooleanAlgebra[String] instance we can use Free
   * to describe what we want to do with it.
   */

  // Using Functions
  val freeProgram0: FreeBooleanAlgebra[String] = or(and(inject("abcd"), not(fls)), not(tru))

  // Using Extension Methods
  val freeProgram1: FreeBooleanAlgebra[String] = inject("abcd") & !fls | !tru

  /*
   * We then define how to convert String to Int
   * Since we do have a BooleanAlgebra[Int]
   */
  def strToInt(b: String): Int = b.length
  implicit val intBoolAlg = IntBooleanAlgebra

  /*
   * Since our program is just data, we can optimize it
   */
  val optFreeProgram1 = optimize(freeProgram1)

  /*
   * We run our program with the conversion to Int
   */
  val out: Int = run(freeProgram1)(strToInt)
  val outOptimized: Int = run(optFreeProgram1)(strToInt)

  println(s"This is our program: $freeProgram1")
  println(s"This is our optimized program: $optFreeProgram1")
  println(s"This is our result: $out")
  println(s"This is our optimized result: $outOptimized")
}
