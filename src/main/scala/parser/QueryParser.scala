package pivot
package parser

import fastparse.*
import fastparse.NoWhitespace.*
import ParseTree.*

object QueryParser:
  def sort[$: P]: P[SubClause] =
    P(identifier ~ " ^").map(SubClause.Sort(_, Order.Asc))
      | P(identifier ~ " v").map(SubClause.Sort(_, Order.Desc))

  def project[$: P]: P[SubClause.Project] =
    P(ValueExprParser.apply).map(expr => SubClause.Project(expr, None))

  def projectAlias[$: P]: P[SubClause.Project] =
    P(identifier ~ ws ~ ":=" ~/ ws ~ ValueExprParser.apply).map((alias, expr) =>
      SubClause.Project(expr, Some(alias))
    )

  def filter[$: P]: P[SubClause.Filter] =
    ValueExprParser.predicate.map(SubClause.Filter.apply)

  def subclause[$: P]: P[SubClause] =
    P(filter | sort | projectAlias | project)

  def clause[$: P]: P[Query.Clause] =
    P("[" ~/ ws ~ subclause ~ ws ~ ("," ~/ ws ~ subclause).rep ~ ws ~ "]")
      .map((head, tail) => Query.Clause(tail.prepended(head)))

  def apply[$: P]: P[Query] =
    P(identifier ~ ws ~ clause.rep).map(Query.apply)
