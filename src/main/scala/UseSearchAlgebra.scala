import boolalgebra.BooleanAlgebra.*
import boolalgebra.BooleanAlgebraF.*
import boolalgebra.instances.{BooleanBooleanAlgebra, PredicateBooleanAlgebra}
import boolalgebra.{BooleanAlgebra, BooleanAlgebraF}
import recursion.Free
import searchalgebra.Keyword.*
import searchalgebra.{Keyword, Site, SiteMetadata}

import java.time.ZonedDateTime
import java.util.function.Predicate

object UseSearchAlgebra extends App {
  given BooleanAlgebra[Predicate[Site]] = PredicateBooleanAlgebra[Site]
  given BooleanAlgebra[Boolean] = BooleanBooleanAlgebra

  val sites: List[Site] = List(
    Site("bob.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah blam bliz", "meow meow meow"),
    Site("flatMap1.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah Scala bliz", "meow meow meow"),
    Site("flatMap2.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah Scala Java", "meow meow meow"),
    Site("flatMap3.com", ZonedDateTime.parse("2018-03-27T05:00:00-04:00"), "blah bliz", "meow spring meow"),
    Site("flatMap4.com", ZonedDateTime.parse("2017-03-27T05:00:00-04:00"), "blah Scala bliz", "meow meow meow")
  )

  val siteMeta: List[SiteMetadata] = sites.map(_.metaOnly)

  val query: Free[BooleanAlgebraF, Keyword] =
    after(ZonedDateTime.parse("2018-01-01T00:00:00Z")) &
      inUrl("flatMap") & (
        term("Scala") |
        !(term("Java") | inText("spring"))
      )

  println(s"This is our search program: $query")

  val search: Predicate[Site] = query.run(Keyword.toPredicate)(BooleanAlgebraF.interpreter)
  val result: List[Site] = sites.filter(search.test)

  println(s"This is our results: $result")

  case class PResult(valid: List[String], remaining: List[(String, Free[BooleanAlgebraF, Keyword])]) {
    def addValid(id: String): PResult = PResult(id :: valid, remaining)
    def addRem(id: String, prg: Free[BooleanAlgebraF, Keyword]): PResult = PResult(valid, (id, prg) :: remaining)
  }

  object PResult {
    val empty: PResult = PResult(List.empty, List.empty)
  }

  val p = siteMeta.foldLeft(PResult.empty) { (p, m) =>
    BooleanAlgebraF.partial(SiteMetadata.partially(m))(query) match {
      case Left(prg) => p.addRem(m.url, prg)
      case Right(a) => if (a) p.addValid(m.url) else p
    }
  }

  println(p)
}
