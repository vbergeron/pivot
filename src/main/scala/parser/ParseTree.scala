package pivot
package parser

sealed trait ParseTree
object ParseTree {

  enum Order:
    case Asc, Desc

  enum SubClause:
    case Filter(predicate: Predicate)
    case Project(expr: ValueExpr, alias: Option[String])
    case Sort(column: String, order: Order)

  case class Query(name: String, clauses: Seq[Query.Clause])

  object Query:
    case class Clause(subclauses: Seq[SubClause])

  enum Type:
    case Int
    case Str
    case Flt

  enum Column:
    case Value(name: String, _type: Type, constraint: Option[Predicate])
    case Ref(relation: String)

  case class Relation(name: String, columns: Seq[Column]) extends ParseTree

  enum JoinType:
    case Inner
    case Left
    case Right
    case Full

  enum SetExpr extends ParseTree:
    case Record(fields: Seq[SetExpr.Field])
    case QueryWrap(query: Query)
    case Union(head: SetExpr, tail: Seq[SetExpr])
    case Diff(head: SetExpr, tail: Seq[SetExpr])
    case PipeTo(source: SetExpr, sink: String)
    case Join(left: SetExpr, right: SetExpr, t: JoinType)

  object SetExpr:
    case class Field(name: Option[String], value: ValueExpr)

  sealed trait ValueExpr
  sealed trait Predicate extends ValueExpr

  object ValueExpr:
    case object Hash                                   extends ValueExpr
    case object Dot                                    extends ValueExpr
    case class Ref(id: String)                         extends ValueExpr
    case class Int(value: Long)                        extends ValueExpr
    case class Str(value: String)                      extends ValueExpr
    case class Greater(lhs: ValueExpr, rhs: ValueExpr) extends Predicate
    case class Lower(lhs: ValueExpr, rhs: ValueExpr)   extends Predicate
    case class Equal(lhs: ValueExpr, rhs: ValueExpr)   extends Predicate
    case class And(lhs: ValueExpr, rhs: ValueExpr)     extends Predicate
    case class Or(lhs: ValueExpr, rhs: ValueExpr)      extends Predicate
    case class Not(expr: ValueExpr)                    extends Predicate
    case class Query(value: ParseTree.Query)           extends ValueExpr

  case class Assign(name: String, value: SetExpr) extends ParseTree
}
