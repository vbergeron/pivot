package pivot
package parser

import fastparse.*
import fastparse.NoWhitespace.*
import ParseTree.*

object SetExprParser:

  def field[$: P]: P[SetExpr.Field] =
    P((identifier ~ ws ~/ ":").? ~ ws ~ ValueExprParser.apply)
      .map(SetExpr.Field.apply)

  def record[$: P]: P[SetExpr.Record] =
    P(
      "{" ~ ws ~ field.repX(
        min = 1,
        sep = ws ~ "," ~/ ws
      ) ~ ws ~ "}"
    )
      .map(SetExpr.Record.apply)

  def query[$: P]: P[SetExpr.QueryWrap] =
    P(QueryParser.apply.map(SetExpr.QueryWrap.apply))

  def parens[$: P]: P[SetExpr] =
    P("(" ~/ diff ~ ")")

  def factor[$: P]: P[SetExpr] =
    P(query | record | parens)

  def union[$: P]: P[SetExpr] =
    P(factor ~ ws ~ ("+" ~/ ws ~ factor ~ ws).rep).map: (head, tail) =>
      if tail.nonEmpty then SetExpr.Union(head, tail) else head

  def diff[$: P]: P[SetExpr] =
    P(union ~ ws ~ ("-" ~/ ws ~ union ~ ws).rep).map: (head, tail) =>
      if tail.nonEmpty then SetExpr.Diff(head, tail) else head

  def pipeTo[$: P]: P[SetExpr] =
    P(diff ~ ws ~ (">" ~/ ws ~ identifier).?)
      .map:
        case (source, Some(sink)) => SetExpr.PipeTo(source, sink)
        case (source, None)       => source

  def apply[$: P]: P[SetExpr] = P(pipeTo)
