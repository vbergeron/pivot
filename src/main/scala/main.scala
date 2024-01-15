package pivot

import executor.Executor
import parser.{ParseTree, ParseTreeParser}
import planner.{LogicalStep, Plan}

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.control

@main
def main(): Unit = {
  @tailrec
  def loop(plan: Plan): Unit =
    print("pivot] ")
    StdIn.readLine() match
      case "quit" =>
      case "run" =>
        Executor.execute(plan)
        loop(Plan(plan.sources, Seq.empty))
      case "rels" =>
        plan.sources.foreach: (rel, _) =>
          println(
            s"${rel.name} ${rel.schema.map(_.fmt).mkString("[", ", ", "]")}"
          )
        loop(plan)
      case line =>
        if line != null && line.nonEmpty then
          val parsed = control.Exception.allCatch.either:
            fastparse.parse(line, ParseTreeParser.file(_)).get.value

          parsed match {
            case Right(value) =>
              loop(LogicalStep.aggregate(plan, value))
            case Left(e) =>
              println(s"ERR: ${e.getClass.getSimpleName} ${e.getMessage}")
              loop(plan)
          }
  loop(Plan.empty)
}
