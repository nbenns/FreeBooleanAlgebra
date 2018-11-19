import boolalgebra.BooleanAlgebraF
import BooleanAlgebraF._
import effect._
import Functor._
import boolalgebra.instances.IntBooleanAlgebra
import recursion.Free
import boolalgebra.BooleanAlgebra._

object UseBooleanAlgebraF extends App {
  /*
   * We create a BooleanAlgebra over Strings by injecting it into the FreeBooleanAlgebra
   * Even though we don't have a BooleanAlgebra[String] instance we can use Free
   * to describe what we want to do with it.
   */

  // Using Functions
  val freeProgram0: Free[BooleanAlgebraF, String] = or(and(inject("abcd"), not(fls[Free[BooleanAlgebraF, String]])), not(tru[Free[BooleanAlgebraF, String]]))

  // Using Extension Methods
  val freeProgram1: Free[BooleanAlgebraF, String] = inject("abcd") & !fls[Free[BooleanAlgebraF, String]] | !tru[Free[BooleanAlgebraF, String]]

  /*
   * We then define how to convert String to Int
   * Since we do have a BooleanAlgebra[Int]
   */
  def strToInt(b: String): Int = b.length
  implicit val intBoolAlg = IntBooleanAlgebra

  /*
   * We can convert our free program of String to one of Int using "convert" and
   * providing a conversion function from String => Int
   */
  val convertedFreeProgram = freeProgram1.map(strToInt)
  val outConverted: Int = interpret(convertedFreeProgram)

  /*
   * But that is really heavy just to interpret it.
   * We can convert and interpret in one step by calling run on our program with the conversion to Int
   */
  val out: Int = run(freeProgram1)(strToInt)

  /*
   * Since our program is just data, we can optimize it first
   * and then run it.
   */
  //val optimizedFreeProgram = freeProgram1.cata(optimizer)
  //val outOptimized: Int = run(optimizedFreeProgram)(strToInt)

  println(s"This is our program: $freeProgram1")
  println(s"This is our converted program: $convertedFreeProgram")
  //println(s"This is our optimized program: $optimizedFreeProgram")
  println(s"This is our result: $out")
  println(s"This is our converted result: $outConverted")
  //println(s"This is our optimized result: $outOptimized")
}
