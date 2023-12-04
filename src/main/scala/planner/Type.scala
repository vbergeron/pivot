package pivot
package planner

import parser.ParseTree

sealed trait Type:
  def castable: Type => Boolean

object Type:

  case object Int extends Type:
    def castable = _ => false
  case object Flt extends Type:
    def castable = _ => false
  case object Str extends Type:
    def castable = _ => false
  case object Bool extends Type:
    def castable = _ => false
  case class RowID(relation: Relation) extends Type:
    def castable =
      case Int => true
      case _   => false

  def fromParsed(parsed: ParseTree.Type): Type = parsed match
    case ParseTree.Type.Int => Int
    case ParseTree.Type.Str => Str
    case ParseTree.Type.Flt => Flt

  def fmt(t: Type): String = t match
    case Type.Int             => "int"
    case Type.Flt             => "flt"
    case Type.Str             => "str"
    case Type.Bool            => "bool"
    case Type.RowID(relation) => s"${relation.name}.#"
