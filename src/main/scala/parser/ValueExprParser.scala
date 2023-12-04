package pivot
package parser

import fastparse.*
import fastparse.NoWhitespace.*
import ParseTree.*

object ValueExprParser:
  def parens[$: P]: P[ValueExpr] =
    P("(" ~/ p0 ~ ")")

  def dot[$: P]: P[ValueExpr.Dot.type]   = P(".").map(_ => ValueExpr.Dot)
  def hash[$: P]: P[ValueExpr.Hash.type] = P("#").map(_ => ValueExpr.Hash)

  def str[$: P]: P[ValueExpr.Str] =
    P("'" ~/ CharsWhile(_ != '\'').! ~ "'").map(ValueExpr.Str.apply)

  def int[$: P]: P[ValueExpr.Int] =
    P(CharIn("0-9").rep(1).!).map(str => ValueExpr.Int(str.toLong))

  def ref[$: P]: P[ValueExpr.Ref] =
    P(identifier).map(ValueExpr.Ref.apply)

  def query[$: P]: P[ValueExpr.Query] =
    P("exec" ~ ws ~ QueryParser.apply).map(ValueExpr.Query.apply)

  def const[$: P]: P[ValueExpr] =
    P(query | ref | str | int | dot | hash | parens)

  def p4[$: P]: P[ValueExpr] =
    P(const ~ ws ~ (">" ~/ ws ~ const ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.Greater(l, r)
      case (l, None)    => l

  def p3[$: P]: P[ValueExpr] =
    P(p4 ~ ws ~ ("<" ~/ ws ~ p4 ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.Lower(l, r)
      case (l, None)    => l

  def p2[$: P]: P[ValueExpr] =
    P(p3 ~ ws ~ ("=" ~/ ws ~ p3 ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.Equal(l, r)
      case (l, None)    => l

  def p1[$: P]: P[ValueExpr] =
    P(p2 ~ ws ~ ("&" ~/ ws ~ p2 ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.And(l, r)
      case (l, None)    => l

  def p0[$: P]: P[ValueExpr] =
    P(p1 ~ ws ~ ("|" ~/ ws ~ p1 ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.Or(l, r)
      case (l, None)    => l

  def apply[$: P]: P[ValueExpr] = p0

  def predicate[$: P]: P[Predicate] =
    apply
      .filter(_.isInstanceOf[Predicate])
      .map(_.asInstanceOf[Predicate])
