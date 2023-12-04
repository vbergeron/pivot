package pivot
package parser
import fastparse.*
import fastparse.NoWhitespace.*
import ParseTree.*

object ParseTreeParser:
  def assign[$: P]: P[ParseTree] =
    P(identifier ~ ws ~ ":=" ~ ws ~ SetExprParser.apply)
      .map(Assign.apply)

  def expr[$: P]: P[ParseTree] =
    P(assign | SchemaParser.apply | SetExprParser.apply)

  def file[$: P]: P[Seq[ParseTree]] =
    P(ws ~ expr.rep(1, ws) ~ End)
