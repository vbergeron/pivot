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

  def fullJoin[$: P]: P[SetExpr] =
    P(factor ~ ws ~ "|><|" ~ ws ~ factor.?).map:
      case (left, Some(right)) => SetExpr.Join(left, right, JoinType.Full)
      case (left, None)        => left

  def rightJoin[$: P]: P[SetExpr] =
    P(fullJoin ~ ws ~ "|>" ~ ws ~ fullJoin.?).map:
      case (left, Some(right)) => SetExpr.Join(left, right, JoinType.Right)
      case (left, None)        => left

  def leftJoin[$: P]: P[SetExpr] =
    P(rightJoin ~ ws ~ "<|" ~ ws ~ rightJoin.?).map:
      case (left, Some(right)) => SetExpr.Join(left, right, JoinType.Left)
      case (left, None)        => left

  def innerJoin[$: P]: P[SetExpr] =
    P(leftJoin ~ ws ~ "<|>" ~ ws ~ leftJoin.?).map:
      case (left, Some(right)) => SetExpr.Join(left, right, JoinType.Inner)
      case (left, None)        => left

  def union[$: P]: P[SetExpr] =
    P(innerJoin ~ ws ~ ("+" ~/ ws ~ innerJoin ~ ws).rep).map: (head, tail) =>
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
