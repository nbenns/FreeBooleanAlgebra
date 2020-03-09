package searchalgebra

import java.time.ZonedDateTime
import boolalgebra.BooleanAlgebraF
import boolalgebra.BooleanAlgebraF.FBAlg

object SiteSearch {
  private final case class Term(t: String) extends Search[Site]
  private final case class After(date: ZonedDateTime) extends Search[Site]
  private final case class InText(t: String) extends Search[Site]
  private final case class InUrl(url: String) extends Search[Site]

  def term(t: String): FBAlg[Search[Site]] = BooleanAlgebraF.inject(Term(t))
  def after(date: ZonedDateTime): FBAlg[Search[Site]] = BooleanAlgebraF.inject(After(date))
  def inText(t: String): FBAlg[Search[Site]] = BooleanAlgebraF.inject(InText(t))
  def inUrl(url: String): FBAlg[Search[Site]] = BooleanAlgebraF.inject(InUrl(url))

  implicit val siteEvaluate: EvaluateSearch[Site] = new EvaluateSearch[Site] {
    override def evalSearch(site: Site)(search: Search[Site]): Boolean = search match {
      case Term(t) => site.terms.contains(t)
      case After(d) => site.indexedAt isAfter d
      case InText(t) => site.text.contains(t)
      case InUrl(url) => site.url.contains(url)
    }
  }
}
