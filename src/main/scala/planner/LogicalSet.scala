package pivot
package planner

import parser.ParseTree
import parser.ParseTree.SetExpr

object LogicalSet:
  case object Empty extends LogicalSet:
    def schema: Seq[Column] = Seq.empty

  case class WrappedQuery(query: Query) extends LogicalSet:
    def schema: Seq[Column] = query.schema

  case class Union(head: LogicalSet, tail: Seq[LogicalSet]) extends LogicalSet:
    def schema: Seq[Column] = head.schema

  case class Diff(head: LogicalSet, tail: Seq[LogicalSet]) extends LogicalSet:
    def schema: Seq[Column] = head.schema

  case class PipeTo(source: LogicalSet, sink: Sink) extends LogicalSet:
    def schema: Seq[Column] = Seq.empty

  object PipeTo:
    def fromParsed(
        catalog: Seq[Relation],
        parsed: ParseTree.SetExpr.PipeTo
    ): PipeTo =
      parsed.sink match
        case "stdout" =>
          PipeTo(
            LogicalSet.fromParsed(catalog, parsed.source, None),
            Sink.Stdout
          )
        case _ => throw Exception("Only stdout sink is supported")

  case class Record(schema: Seq[Column], fields: Seq[TypedExpr])
      extends LogicalSet

  def fromParsed(
      catalog: Seq[Relation],
      parsed: ParseTree.SetExpr,
      implicitSchema: Option[Seq[Column]]
  ): LogicalSet =
    parsed match
      case SetExpr.Record(fields) =>
        implicitSchema match
          case Some(schema) if fields.exists(_.name.nonEmpty) =>
            throw Exception(
              s"Could not apply inherited schema $schema over $fields: at least one field is named"
            )
          case Some(schema) =>
            val typed = fields.map(field =>
              TypedExpr.fromUntyped(field.value, Seq.empty, None, None)
            )
            Record(schema, typed)
          case None =>
            val typed = fields.map(field =>
              val expr =
                TypedExpr.fromUntyped(field.value, Seq.empty, None, None)
              (
                Column(field.name.getOrElse(TypedExpr.name(expr)), expr._type),
                expr
              )
            )
            val schema = typed.map((column, _) => column)
            val values = typed.map((_, value) => value)
            Record(schema, values)

      case SetExpr.QueryWrap(query) =>
        WrappedQuery(Query.fromParsed(catalog, query))

      case SetExpr.Union(head, tail) =>
        val logicalHead = fromParsed(catalog, head, implicitSchema)
        val logicalTail =
          tail.map(fromParsed(catalog, _, Some(logicalHead.schema)))
        if logicalTail.forall(_.schema == logicalHead.schema)
        then Union(logicalHead, logicalTail)
        else
          val mismatchs = logicalTail.filter(_.schema != logicalHead.schema)
          throw Exception(
            s"Schema missmatch: head is ${logicalHead.schema}, tail contains $mismatchs"
          )

      case SetExpr.Diff(head, tail) =>
        val logicalHead = fromParsed(catalog, head, implicitSchema)
        val logicalTail =
          tail.map(fromParsed(catalog, _, Some(logicalHead.schema)))
        if logicalTail.forall(_.schema == logicalHead.schema)
        then Diff(logicalHead, logicalTail)
        else
          val mismatchs = logicalTail.filter(_.schema != logicalHead.schema)
          throw Exception(
            s"Schema missmatch: head is ${logicalHead.schema}, tail contains $mismatchs"
          )

      case it: SetExpr.PipeTo => PipeTo.fromParsed(catalog, it)

sealed trait LogicalSet:
  def schema: Seq[Column]
