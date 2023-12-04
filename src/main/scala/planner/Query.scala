package pivot
package planner

import parser.ParseTree

case class Query(relation: Relation, clauses: Seq[Clause]):
  def addClause(clause: Clause): Query =
    copy(clauses = clauses.appended(clause))

  def schema: Seq[Column] =
    if clauses.isEmpty then relation.schema
    else clauses.last.schema

object Query:
  def from(relation: Relation): Query = Query(relation, Seq.empty)

  def fromParsed(catalog: Seq[Relation], parsed: ParseTree.Query): Query =
    val relation = catalog.find(_.name == parsed.name) match
      case Some(found) => found
      case None => throw Exception(s"Could not find relation ${parsed.name}")

    val (q, _) =
      parsed.clauses.foldLeft((Query.from(relation), relation)):
        case ((query, relation), parsedClause) =>
          val clause = Clause.fromParsed(relation, parsedClause)
          (query.addClause(clause), Relation("", clause.schema))
    q
