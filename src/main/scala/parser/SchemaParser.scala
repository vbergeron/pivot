package pivot
package parser

import fastparse.*
import fastparse.NoWhitespace.*
import ParseTree.*

object SchemaParser:
  def _type[$: P]: P[Type] =
    import Type.*
    P("int").map(_ => Int)
      | P("str").map(_ => Str)
      | P("flt").map(_ => Flt)

  def column[$: P]: P[Column] =
    def constraint[$: P]: P[Predicate] =
      P("[" ~/ ws ~ ValueExprParser.predicate ~ ws ~ "]")
        .filter(_.isInstanceOf[Predicate])
        .map(_.asInstanceOf[Predicate])

    P(identifier ~ ws ~ ":" ~/ ws ~ _type ~ ws ~ constraint.?)
      .map(Column.Value.apply)
      | P(identifier ~/ "." ~/ "#").map(Column.Ref.apply)

  def apply[$: P]: P[Relation] =
    P(
      "rel" ~/ ws ~ identifier ~ ws ~/ "{" ~ ws ~ column.repX(
        min = 1,
        sep = ws ~ "," ~/ ws
      ) ~ ws ~ "}"
    )
      .map(Relation.apply)
