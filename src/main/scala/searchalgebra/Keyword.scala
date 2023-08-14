package searchalgebra

import boolalgebra.BooleanAlgebraF
import effect.Functor.*
import recursion.Free

import java.time.ZonedDateTime
import java.util.function.Predicate

enum Keyword {
  case Term(t: String)
  case After(date: ZonedDateTime)
  case InText(t: String)
  case InUrl(url: String)
}

object Keyword {
  def term(t: String): Free[BooleanAlgebraF, Keyword] = BooleanAlgebraF.inject(Keyword.Term(t))
  def after(date: ZonedDateTime): Free[BooleanAlgebraF, Keyword] = BooleanAlgebraF.inject(Keyword.After(date))
  def inText(t: String): Free[BooleanAlgebraF, Keyword] = BooleanAlgebraF.inject(Keyword.InText(t))
  def inUrl(url: String): Free[BooleanAlgebraF, Keyword] = BooleanAlgebraF.inject(Keyword.InUrl(url))

  def toPredicate: Keyword => Predicate[Site] = {
    case Keyword.Term(t) => site => site.terms.contains(t)
    case Keyword.After(d) => site => site.indexedAt.isAfter(d)
    case Keyword.InText(t) => site => site.text.contains(t)
    case Keyword.InUrl(url) => site => site.url.contains(url)
  }
}
