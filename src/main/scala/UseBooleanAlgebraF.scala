import boolalgebra.BooleanAlgebra.*
import boolalgebra.BooleanAlgebraF.*
import boolalgebra.instances.IntBooleanAlgebra
import boolalgebra.{BooleanAlgebra, BooleanAlgebraF}
import effect.Functor.*
import recursion.Free
import recursion.Free.*

object UseBooleanAlgebraF extends App {
  /*
   * We create a BooleanAlgebra over Strings by injecting it into the FreeBooleanAlgebra
   * Even though we don't have a BooleanAlgebra[String] instance we can use Free
   * to describe what we want to do with it.
   */

  private object Example {
    import BooleanAlgebraF.given BooleanAlgebra[Free[BooleanAlgebraF, String]]

    // Using Functions
    val freeProgram0: Free[BooleanAlgebraF, String] = or(and(inject("abcd"), not(fls)), not(tru))

    // Using Extension Methods
    val freeProgram1: Free[BooleanAlgebraF, String] = inject("abcd") & !fls | !tru
  }

  /*
   * We then define how to convert String to Int
   * Since we do have a BooleanAlgebra[Int]
   */
  def strToInt(b: String): Int = b.length
  given BooleanAlgebra[Int] = IntBooleanAlgebra

  /*
   * We can convert our free program of String to one of Int using "convert" and
   * providing a conversion function from String => Int
   */
  val convertedFreeProgram: Free[BooleanAlgebraF, Int] = Example.freeProgram1.map(strToInt)
  val outConverted: Int = convertedFreeProgram.cata(BooleanAlgebraF.interpreter)

  /*
   * But that is really heavy just to interpret it.
   * We can convert and interpret in one step by calling run on our program with the conversion to Int
   */
  val out: Int = Example.freeProgram1.run(strToInt)(BooleanAlgebraF.interpreter)

  /*
   * Since our program is just data, we can optimize it first
   * and then run it.
   */
  val optimizedFreeProgram = optimize(Example.freeProgram1)
  val outOptimized: Int = optimizedFreeProgram.run(strToInt)(BooleanAlgebraF.interpreter)

  println(s"This is our program: ${Example.freeProgram1}")
  println(s"This is our converted program: $convertedFreeProgram")
  println(s"This is our optimized program: $optimizedFreeProgram")
  println(s"This is our result: $out")
  println(s"This is our converted result: $outConverted")
  println(s"This is our optimized result: $outOptimized")
}
