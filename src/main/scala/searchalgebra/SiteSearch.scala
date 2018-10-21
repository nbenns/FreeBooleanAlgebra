package searchalgebra

import java.time.ZonedDateTime
import boolalgebra.FreeBooleanAlgebra

object SiteSearch {
  private case class Term(t: String) extends Search[Site]
  private case class After(date: ZonedDateTime) extends Search[Site]
  private case class InText(t: String) extends Search[Site]
  private case class InUrl(url: String) extends Search[Site]

  def term(t: String): FreeBooleanAlgebra[Search[Site]] = FreeBooleanAlgebra.inject(Term(t))
  def after(date: ZonedDateTime): FreeBooleanAlgebra[Search[Site]] = FreeBooleanAlgebra.inject(After(date))
  def inText(t: String): FreeBooleanAlgebra[Search[Site]] = FreeBooleanAlgebra.inject(InText(t))
  def inUrl(url: String): FreeBooleanAlgebra[Search[Site]] = FreeBooleanAlgebra.inject(InUrl(url))

  implicit val siteEvaluate: EvaluateSearch[Site] = new EvaluateSearch[Site] {
    override def evalSearch(site: Site)(search: Search[Site]): Boolean = search match {
      case Term(t) => site.terms.contains(t)
      case After(d) => site.indexedAt isAfter d
      case InText(t) => site.text.contains(t)
      case InUrl(url) => site.url.contains(url)
    }
  }
}
