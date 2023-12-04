package pivot
package planner

case class Column(name: String, _type: Type):
  def fmt: String =
    _type match
      case _: Type.RowID => name
      case _             => s"$name($_type)"
