package searchalgebra

import java.time.ZonedDateTime

case class Site(url: String, indexedAt: ZonedDateTime, terms: String, text: String)
