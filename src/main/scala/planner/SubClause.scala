package pivot
package planner

import parser.ParseTree

enum SubClause:
  case Filter(predicate: TypedExpr)
  case Project(expr: TypedExpr, field: Column)
  case Sort(field: Column, order: Order)

object SubClause:
  def fromParsed(
      catalog: Seq[Relation],
      relation: Relation,
      parsed: ParseTree.SubClause
  ): SubClause =
    parsed match
      case ParseTree.SubClause.Filter(predicate) =>
        SubClause.Filter(
          TypedExpr.fromUntyped(
            predicate,
            catalog,
            relation.schema,
            None,
            Some(relation)
          )
        )
      case ParseTree.SubClause.Project(expr, alias) =>
        val typed =
          TypedExpr.fromUntyped(
            expr,
            catalog,
            relation.schema,
            None,
            Some(relation)
          )
        SubClause.Project(
          typed,
          Column(alias.getOrElse(TypedExpr.name(typed)), typed.t)
        )
      case ParseTree.SubClause.Sort(column, order) =>
        relation.schema.find(_.name == column) match
          case Some(column) => SubClause.Sort(column, Order.fromParsed(order))
          case None         => throw Exception(s"Column $column not found")
