package pivot
package planner

sealed trait Sink
object Sink:
  case object Stdout extends Sink
