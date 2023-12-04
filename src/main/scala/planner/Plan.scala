package pivot
package planner

case class Plan(
    sources: Map[Relation, LogicalSet],
    effects: Seq[LogicalSet.PipeTo]
):
  def addSource(
      relation: Relation,
      source: LogicalSet
  ): Plan =
    copy(sources = sources.updated(relation, source))

  def addEffect(effect: LogicalSet.PipeTo): Plan =
    copy(effects = effects.appended(effect))

  def catalog: Seq[Relation] = sources.keys.toSeq

object Plan:
  val empty: Plan = Plan(Map.empty, Seq.empty)
