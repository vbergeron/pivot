package pivot
package planner

case class Column(name: String, t: Type):
  def fmt: String =
    t match
      case _: Type.RowID => name
      case _             => s"$name($t)"
