import boolalgebra.BooleanAlgebraF
import boolalgebra.BooleanAlgebraF.*
import recursion.{CoFAlgebra, FAlgebra, Free}
import effect.Functor.*

object TryAna extends App {
  val parse: CoFAlgebra[BooleanAlgebraF, Int] = { i =>
    if (i < 50) And(i + 1, i + 2)
    else if (i % 2 == 0) Tru
    else Fls
  }

  val meh: FAlgebra[BooleanAlgebraF, String] = {
    case Tru => "t"
    case Fls => "f"
    case Not(a) => "! " + a
    case And(a, b) => a + " & " + b
    case Or(a, b) => a + " | " + b
  }

//  val prg = Free.ana[BooleanAlgebraF, Int](0, parse)
//
//  println(prg)

//  val res = prg.map(_.toString).cata(meh)
//  println(res)
//
//  println()

  val hres = Free.hylo(0, parse, meh)
  println(hres)
}
