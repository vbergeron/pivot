package pivot

import fastparse.*
import NoWhitespace.*

def identifier[$: P]: P[String] =
  P(CharPred(_.isLetter).rep.!).filter(_.nonEmpty)

enum Order:
  case Asc, Desc

def order[$: P]: P[Order] =
  P(" ^").map(_ => Order.Asc) | P(" v").map(_ => Order.Desc)

enum SubClause:
  case Filter(predicate: Predicate)
  case Project(column: String, order: Option[Order])

object SubClauseParser:
  import SubClause.*
  def project[$: P]: P[Project] =
    (identifier ~ order.?).map(Project.apply)

  def filter[$: P]: P[Filter] =
    ValueExprParser.predicate.map(Filter.apply)

  def apply[$: P]: P[SubClause] =
    P(filter | project)

def clause[$: P]: P[Seq[SubClause]] =
  P(
    "[" ~/ ws ~ SubClauseParser.apply ~ ("," ~/ SubClauseParser.apply).rep ~ "]"
  ).map((head, tail) => tail.prepended(head))

enum Type:
  case Nat
  case Int
  case Str
  case Flt

enum Column:
  case Value(name: String, _type: Type, constraint: Option[Predicate])
  case Ref(relation: String)

case class Relation(name: String, columns: Seq[Column])

def _type[$: P]: P[Type] =
  import Type.*
  P("nat").map(_ => Nat)
    | P("int").map(_ => Int)
    | P("str").map(_ => Str)
    | P("flt").map(_ => Flt)

def ws[$: P]: P[Unit] =
  CharPred(_.isWhitespace).rep

def column[$: P]: P[Column] =
  def constraint[$: P]: P[Predicate] =
    P("[" ~/ ws ~ ValueExprParser.predicate ~ ws ~ "]")

  P(identifier ~ ws ~ ":" ~/ ws ~ _type ~ ws ~ constraint.?)
    .map(Column.Value.apply)
    | P(identifier ~/ "." ~/ "#").map(Column.Ref.apply)

def relation[$: P]: P[Relation] =
  P(
    "rel" ~/ ws ~ identifier ~ ws ~/ "{" ~ ws ~ column.repX(
      min = 1,
      sep = ws ~ "," ~/ ws
    ) ~ ws ~ "}"
  )
    .map(Relation.apply)

case class Field(name: Option[String], value: Value)

enum SetExpr:
  case Record(fields: Seq[Field])
  case Query(name: String, clauses: Seq[Seq[SubClause]])
  case Union(members: Seq[SetExpr])
  case Diff(members: Seq[SetExpr])

def field[$: P]: P[Field] =
  P((identifier ~ ws ~/ ":").? ~ ws ~ value).map(Field.apply)

def record[$: P]: P[SetExpr.Record] =
  P(
    "{" ~ ws ~ field.repX(
      min = 1,
      sep = ws ~ "," ~/ ws
    ) ~ ws ~ "}"
  )
    .map(SetExpr.Record.apply)

def query[$: P]: P[SetExpr.Query] =
  (identifier ~ clause.rep).map(SetExpr.Query.apply)

def parens[$: P]: P[SetExpr] =
  P("(" ~/ diff ~ ")")

def factor[$: P]: P[SetExpr] =
  P(query | record | parens)

def union[$: P]: P[SetExpr] =
  P(factor ~ ws ~ ("+" ~/ ws ~ factor ~ ws).rep).map((head, tail) =>
    if tail.nonEmpty
    then SetExpr.Union(tail.prepended(head))
    else head
  )

def diff[$: P]: P[SetExpr] =
  P(union ~ ws ~ ("-" ~/ ws ~ union ~ ws).rep).map((head, tail) =>
    if tail.nonEmpty
    then SetExpr.Diff(tail.prepended(head))
    else head
  )

def setExpr[$: P]: P[SetExpr] = P(diff)

enum Value:
  case Int(value: Long)
  case Str(value: String)

def value[$: P]: P[Value] =
  P("'" ~/ CharsWhile(_ != '\'').! ~ "'").map(Value.Str.apply)
    | P(CharIn("0-9").rep(1).!).map(str => Value.Int(str.toLong))

sealed trait ValueExpr
sealed trait Predicate extends ValueExpr
object ValueExpr:
  case object Dot                                    extends ValueExpr
  case class Ref(id: String)                         extends ValueExpr
  case class Const(value: Value)                     extends ValueExpr
  case class Greater(lhs: ValueExpr, rhs: ValueExpr) extends Predicate
  case class Lower(lhs: ValueExpr, rhs: ValueExpr)   extends Predicate
  case class Equal(lhs: ValueExpr, rhs: ValueExpr)   extends Predicate

object ValueExprParser:
  def parens[$: P]: P[ValueExpr] =
    P("(" ~/ p0 ~ ")")

  def dot[$: P]: P[ValueExpr.Dot.type] = P(".").map(_ => ValueExpr.Dot)

  def const[$: P]: P[ValueExpr] =
    P(identifier.map(ValueExpr.Ref) | value.map(ValueExpr.Const) | dot | parens)

  def p2[$: P]: P[ValueExpr] =
    P(const ~ ws ~ (">" ~/ ws ~ const ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.Greater(l, r)
      case (l, None)    => l

  def p1[$: P]: P[ValueExpr] =
    P(p2 ~ ws ~ ("<" ~/ ws ~ p2 ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.Lower(l, r)
      case (l, None)    => l

  def p0[$: P]: P[ValueExpr] =
    P(p1 ~ ws ~ ("=" ~/ ws ~ p1 ~ ws).?).map:
      case (l, Some(r)) => ValueExpr.Equal(l, r)
      case (l, None)    => l

  def apply[$: P]: P[ValueExpr] = p0

  def predicate[$: P]: P[Predicate] =
    apply
      .filter(_.isInstanceOf[Predicate])
      .map(_.asInstanceOf[Predicate])

@main
def main(): Unit = {
  println(parse("id", identifier(_)))
  println(parse(" ^", order(_)))
  println(parse(" v", order(_)))
  println(parse(" v", order(_)))
  println(parse("name", SubClauseParser(_)))
  println(parse("name ^", SubClauseParser(_)))
  println(parse("name v", SubClauseParser(_)))
  println(parse("age > 18", SubClauseParser(_)))
  println(parse("[name]", clause(_)))
  println(parse("[name,age ^,salary v]", clause(_)))
  // println(parse("[name, age ^, salary v]", clause(_), verboseFailures = true))
  println(parse("Employees [name][age ^][salary v]", query(_)))

  println(parse("nat", _type(_)))
  println(parse("int", _type(_)))
  println(parse("str", _type(_)))
  println(parse("flt", _type(_)))

  println(parse("name: str", column(_)))
  println(parse("Employee.#", column(_)))
  println(
    parse("rel Employee { name: str, age: nat [. < 18], Dept.# }", relation(_))
  )

  println(parse("some: 'foo'", field(_)))
  println(parse("{name: 'foo', age: 28}", setExpr(_)))
  println(parse("Employees + Employees", setExpr(_)))
  println(
    parse(
      """Employee + {name: 'foo', age: 18} + {name: 'bar', age: 52}
      |""".stripMargin,
      setExpr(_)
    )
  )
  println(
    parse(
      """Employee - {name: 'foo', age: 18} + {name: 'bar', age: 52}
        |""".stripMargin,
      setExpr(_)
    )
  )

  println(parse("'foo bar barred'", value(_)))
  println(parse("123456", value(_)))

  println(parse("foo < 2", ValueExprParser.apply(_)))

}
