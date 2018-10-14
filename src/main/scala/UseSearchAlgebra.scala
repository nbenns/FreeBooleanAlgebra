import java.time.ZonedDateTime

import searchalgebra.Search._
import boolalgebra.BooleanAlgebra._
import boolalgebra.FreeBooleanAlgebra
import searchalgebra.{Search, Site}

object UseSearchAlgebra extends App {
  val Sites: List[Site] = List(
    Site("bob.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah blam bliz", "meow meow meow"),
    Site("flatMap1.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah Scala bliz", "meow meow meow"),
    Site("flatMap2.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah Scala Java", "meow meow meow"),
    Site("flatMap3.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah Scala bliz", "meow spring meow"),
    Site("flatMap4.com", ZonedDateTime.parse("2017-03-27T05:00:00-04:00"), "blah Scala bliz", "meow meow meow")
  )

  val search: FreeBooleanAlgebra[Search] =
    term("Scala") &
    after(ZonedDateTime.parse("2018-01-01T00:00:00Z")) &
    !(term("Java") | inText("spring")) &
    inUrl("flatMap")

  println(s"This is our search program: $search")

  val result = Sites.filter(evalSearch(search))

  println(s"This is our results: $result")
}
