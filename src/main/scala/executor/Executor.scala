package pivot
package executor

import planner.*

import scala.annotation.tailrec

object Executor:

  final case class Ctx(sources: Map[Relation, LogicalSet], schema: Seq[Column]):
    def eval(expr: TypedExpr): Row => Cell = row =>
      import TypedExpr.*
      def rec(expr: TypedExpr): Cell = eval(expr)(row)

      expr match
        case Cast(expr, to)    => rec(expr)
        case RowID(_)          => Cell.Int(row.id)
        case Ref(column)       => row.cells(schema.indexOf(column))
        case Int(value)        => Cell.Int(value)
        case Str(value)        => Cell.Str(value)
        case Bool(value)       => Cell.Bool(value)
        case Greater(lhs, rhs) => Cell.Bool(Cell.gt(rec(lhs), rec(rhs)))
        case Lower(lhs, rhs)   => Cell.Bool(Cell.lt(rec(lhs), rec(rhs)))
        case Equal(lhs, rhs)   => Cell.Bool(rec(lhs) == rec(rhs))
        case And(lhs, rhs)     => Cell.Bool(Cell.and(rec(lhs), rec(rhs)))
        case Or(lhs, rhs)      => Cell.Bool(Cell.or(rec(lhs), rec(rhs)))
        case Not(expr)         => Cell.Bool(Cell.not(rec(expr)))
        case AQuery(q) =>
          val rows = executeLogicalSet(sources, LogicalSet.WrappedQuery(q))
          if rows.length > 1 then
            throw Exception("Query expression returned more than one row")
          rows.head.cells.head

    def compileFilters(filters: Seq[SubClause.Filter]): Row => Boolean = row =>
      val factors = filters.map(f => eval(f.predicate))
      factors.forall(_(row) == Cell.Bool(true))

    def compileProjects(projects: Seq[SubClause.Project]): Row => Row =
      val factors = projects.map(p => eval(p.expr))
      row => Row(row.id, factors.map(_(row)))

    def executeSink(sink: Sink): Row => Unit =
      sink match
        case Sink.Stdout =>
          println(schema.map(_.fmt).mkString("[", ", ", "]"))
          val fmt: Cell => String =
            case Cell.Str(value)  => s"'$value'"
            case Cell.Int(value)  => value.toString
            case Cell.Bool(value) => value.toString
            case Cell.Flt(value)  => value.toString

          row => println(row.cells.map(fmt).mkString("> ", ", ", ""))

    def compileSort(sorts: Seq[SubClause.Sort]): (Row, Row) => Boolean =
      // lt function for use in sortWith
      val steps = sorts.map: sort =>
        (sort.order, schema.indexOf(sort.field))

      @tailrec
      def rec(r1: Row, r2: Row, steps: List[(Order, Int)]): Boolean =
        steps match
          case (order, idx) :: tail =>
            val l = r1.cells(idx)
            val r = r2.cells(idx)
            if l == r then rec(r1, r2, tail)
            else
              order match
                case Order.Asc  => Cell.lt(l, r)
                case Order.Desc => Cell.gt(l, r)
          case Nil => false

      rec(_, _, steps.toList)

  def executeLogicalSet(
      sources: Map[Relation, LogicalSet],
      set: LogicalSet
  ): Seq[Row] =
    set match
      case LogicalSet.Empty => Seq.empty

      case LogicalSet.WrappedQuery(query) =>
        val from = executeLogicalSet(sources, sources(query.relation))
        val (rs, _) = query.clauses.foldLeft((from, query.relation.schema)):
          case ((acc, schema), clause) =>
            val ctx = Ctx(sources, schema)
            val rows = acc.iterator.zipWithIndex
              .map((row, i) => row.copy(id = i))
              .filter(ctx.compileFilters(clause.filters))
              .map(ctx.compileProjects(clause.projects))
              .toSeq
            if clause.sorts.nonEmpty
            then (rows.sortWith(ctx.compileSort(clause.sorts)), clause.schema)
            else (rows, clause.schema)
        rs

      case LogicalSet.Union(head, tail) =>
        tail.foldLeft(executeLogicalSet(sources, head)): (acc, e) =>
          acc.appendedAll(executeLogicalSet(sources, e) diff acc)

      case LogicalSet.Diff(head, tail) =>
        tail.foldLeft(executeLogicalSet(sources, head)): (acc, e) =>
          acc diff executeLogicalSet(sources, e)

      case LogicalSet.PipeTo(source, sink) =>
        val ctx = Ctx(sources, source.schema)
        executeLogicalSet(sources, source)
          .tapEach(ctx.executeSink(sink))

      case LogicalSet.Record(schema, fields) =>
        val ctx = Ctx(sources, Seq.empty)
        Seq(Row(0, fields.map(e => ctx.eval(e)(Row(0, Seq.empty)))))

  def execute(plan: planner.Plan): Unit =
    plan.effects.foreach(executeLogicalSet(plan.sources, _))
