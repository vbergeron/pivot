package pivot
package planner

import parser.ParseTree

case class Clause(
    projects: Seq[SubClause.Project],
    filters: Seq[SubClause.Filter],
    sorts: Seq[SubClause.Sort]
):
  def addProject(project: SubClause.Project): Clause =
    copy(projects = projects.appended(project))

  def addFilter(filter: SubClause.Filter): Clause =
    copy(filters = filters.appended(filter))

  def addSort(sort: SubClause.Sort): Clause =
    copy(sorts = sorts.appended(sort))

  def schema: Seq[Column] = projects.map(_.field)

object Clause:
  val empty: Clause = Clause(Seq.empty, Seq.empty, Seq.empty)

  def fromParsed(
      catalog: Seq[Relation],
      relation: Relation,
      parsed: ParseTree.Query.Clause
  ): Clause =
    val clause = parsed.subclauses.foldLeft(empty): (clause, elem) =>
      SubClause.fromParsed(catalog, relation, elem) match
        case it: SubClause.Project => clause.addProject(it)
        case it: SubClause.Filter  => clause.addFilter(it)
        case it: SubClause.Sort    => clause.addSort(it)
    if clause.projects.nonEmpty
    then clause
    else
      relation.schema.foldLeft(clause): (cl, col) =>
        cl.addProject(SubClause.Project(TypedExpr.Ref(col), col))
