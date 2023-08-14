package searchalgebra

import java.time.ZonedDateTime

case class SiteMetadata(url: String, indexedAt: ZonedDateTime, terms: String)

object SiteMetadata {
  def partially(meta: SiteMetadata)(k: Keyword): Option[Boolean] = k match {
    case Keyword.Term(t) => Some(meta.terms.contains(t))
    case Keyword.After(date) => Some(meta.indexedAt.isAfter(date))
    case Keyword.InUrl(url) => Some(meta.url.contains(url))
    case Keyword.InText(t) => None
  }
}
