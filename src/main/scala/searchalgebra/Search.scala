package searchalgebra

import java.time.ZonedDateTime

import boolalgebra.{BooleanAlgebra, FreeBooleanAlgebra}
import boolalgebra.FreeBooleanAlgebra._
import boolalgebra.instances.BooleanBooleanAlgebra

sealed trait Search

object Search {
  private case class Term(t: String) extends Search
  private case class After(date: ZonedDateTime) extends Search
  private case class InText(t: String) extends Search
  private case class InUrl(url: String) extends Search

  def term(t: String): FreeBooleanAlgebra[Search] = inject(Term(t))
  def after(date: ZonedDateTime): FreeBooleanAlgebra[Search] = inject(After(date))
  def inText(t: String): FreeBooleanAlgebra[Search] = inject(InText(t))
  def inUrl(url: String): FreeBooleanAlgebra[Search] = inject(InUrl(url))

  def evalSearch(pred: FreeBooleanAlgebra[Search])(site: Site): Boolean = {
    implicit val alg: BooleanAlgebra[Boolean] = BooleanBooleanAlgebra

    def searchToBool(s: Search): Boolean = s match {
      case Term(t) => site.terms.contains(t)
      case After(d) => site.indexedAt isAfter d
      case InText(t) => site.text.contains(t)
      case InUrl(url) => site.url.contains(url)
    }

    FreeBooleanAlgebra.run(pred)(searchToBool)
  }
}
