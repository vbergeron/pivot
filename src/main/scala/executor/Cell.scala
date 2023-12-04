package pivot
package executor

enum Cell:
  case Str(value: String)
  case Int(value: Long)
  case Bool(value: Boolean)
  case Flt(value: Double)

object Cell:
  def lt(lhs: Cell, rhs: Cell): Boolean =
    (lhs, rhs) match
      case (Int(l), Int(r))   => l < r
      case (Flt(l), Flt(r))   => l < r
      case (Str(l), Str(r))   => l.compareTo(r) < 0
      case (Bool(l), Bool(r)) => !l && r
      case _                  => false

  def gt(lhs: Cell, rhs: Cell): Boolean =
    (lhs, rhs) match
      case (Int(l), Int(r))   => l > r
      case (Flt(l), Flt(r))   => l > r
      case (Str(l), Str(r))   => l.compareTo(r) > 0
      case (Bool(l), Bool(r)) => l && !r
      case _                  => false

  def and(lhs: Cell, rhs: Cell): Boolean =
    (lhs, rhs) match
      case (Bool(l), Bool(r)) => l && r
      case _                  => throw Exception("unreachable")

  def or(lhs: Cell, rhs: Cell): Boolean =
    (lhs, rhs) match
      case (Bool(l), Bool(r)) => l || r
      case _                  => throw Exception("unreachable")

  def not(expr: Cell): Boolean =
    expr match
      case Bool(it) => !it
      case _        => throw Exception("unreachable")
