package pivot
package planner

import parser.ParseTree

case class Relation(name: String, schema: Seq[Column]):
  def fmt: String =
    s"$name [${schema.map(_.fmt).mkString("[", ", ", "]")}]"

object Relation:
  def fromParsed(parsed: ParseTree.Relation): Relation =
    val columns = parsed.columns.map:
      case ParseTree.Column.Value(name, _type, _) =>
        Column(name, Type.fromParsed(_type))
      case _: pivot.parser.ParseTree.Column.Ref =>
        throw Exception("Table reference not yet supported")
    Relation(parsed.name, columns)
