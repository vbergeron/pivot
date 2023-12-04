package pivot

import executor.Executor
import parser.{ParseTree, ParseTreeParser}
import planner.{LogicalStep, Plan}

import scala.io.StdIn

@main
def main(): Unit = {
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
          try
            val parsed =
              fastparse.parse(line, ParseTreeParser.file(_)).get.value
            loop(LogicalStep.aggregate(plan, parsed))
          catch
            case e: Throwable =>
              println(s"ERR: ${e.getClass.getSimpleName} ${e.getMessage}")
              loop(plan)
  loop(Plan.empty)
}
