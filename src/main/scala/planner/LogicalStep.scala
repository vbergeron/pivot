package pivot
package planner

import parser.ParseTree

object LogicalStep:

  private def aggregateOne(previous: Plan, parsed: ParseTree): Plan =
    parsed match
      case it: ParseTree.Relation =>
        previous.addSource(Relation.fromParsed(it), LogicalSet.Empty)
      case it: ParseTree.SetExpr.PipeTo =>
        previous.addEffect(LogicalSet.PipeTo.fromParsed(previous.catalog, it))
      case it: ParseTree.Assign =>
        val logicalSet = LogicalSet.fromParsed(previous.catalog, it.value, None)
        previous.addSource(
          Relation(it.name, logicalSet.schema),
          logicalSet
        )
      case _: ParseTree.SetExpr =>
        throw Exception("Non-final set expression")

  def aggregate(plan: Plan, trees: Seq[ParseTree]): Plan =
    trees.foldLeft(plan)(aggregateOne)
