package pivot
package planner

import parser.ParseTree

enum Order:
  case Asc, Desc

object Order:
  def fromParsed(parsed: ParseTree.Order): Order =
    parsed match
      case ParseTree.Order.Asc  => Order.Asc
      case ParseTree.Order.Desc => Order.Desc
