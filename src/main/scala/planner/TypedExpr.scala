package pivot
package planner

import parser.ParseTree
import parser.ParseTree.ValueExpr

sealed trait TypedExpr:
  def _type: Type

object TypedExpr:
  case class RowID(relation: Relation) extends TypedExpr:
    def _type = Type.RowID(relation)

  case class Ref(column: Column) extends TypedExpr:
    def _type = column._type

  case class Int(value: Long) extends TypedExpr:
    def _type = Type.Int

  case class Str(value: String) extends TypedExpr:
    def _type = Type.Str

  case class Bool(value: Boolean) extends TypedExpr:
    def _type = Type.Bool

  case class Greater(lhs: TypedExpr, rhs: TypedExpr) extends TypedExpr:
    def _type = Type.Bool

  case class Lower(lhs: TypedExpr, rhs: TypedExpr) extends TypedExpr:
    def _type = Type.Bool

  case class Equal(lhs: TypedExpr, rhs: TypedExpr) extends TypedExpr:
    def _type = Type.Bool

  case class And(lhs: TypedExpr, rhs: TypedExpr) extends TypedExpr:
    def _type = Type.Bool

  case class Or(lhs: TypedExpr, rhs: TypedExpr) extends TypedExpr:
    def _type = Type.Bool

  case class Not(expr: TypedExpr) extends TypedExpr:
    def _type = Type.Bool

  case class Cast(expr: TypedExpr, to: Type) extends TypedExpr:
    def _type = to

  def fromUntyped(
      expr: ParseTree.ValueExpr,
      schema: Seq[Column],
      implicitColumn: Option[Column],
      implicitRelation: Option[Relation]
  ): TypedExpr =
    def operator(lhs: ParseTree.ValueExpr, rhs: ParseTree.ValueExpr)(
        make: (TypedExpr, TypedExpr) => TypedExpr
    ): TypedExpr =
      val (l, r) = (rec(lhs), rec(rhs))
      if l._type == r._type then make(l, r)
      else if l._type castable r._type then make(Cast(l, r._type), r)
      else if r._type castable l._type then make(l, Cast(r, l._type))
      else throw Exception("type error")

    def rec(expr: ParseTree.ValueExpr): TypedExpr =
      expr match
        case ValueExpr.Greater(lhs, rhs) => operator(lhs, rhs)(Greater.apply)
        case ValueExpr.Lower(lhs, rhs)   => operator(lhs, rhs)(Lower.apply)
        case ValueExpr.Equal(lhs, rhs)   => operator(lhs, rhs)(Equal.apply)
        case ValueExpr.And(lhs, rhs) =>
          val (l, r) = (rec(lhs), rec(rhs))
          if l._type != Type.Bool then throw Exception("type error")
          if r._type != Type.Bool then throw Exception("type error")
          And(l, r)

        case ValueExpr.Or(lhs, rhs) =>
          val (l, r) = (rec(lhs), rec(rhs))
          if l._type != Type.Bool then throw Exception("type error")
          if r._type != Type.Bool then throw Exception("type error")
          Or(l, r)

        case ValueExpr.Dot =>
          implicitColumn match
            case Some(column) => Ref(column)
            case None         => throw Exception("No implicit column present")

        case ValueExpr.Hash =>
          implicitRelation match
            case Some(relation) => RowID(relation)
            case None => throw Exception("No implicit relation present")

        case ValueExpr.Ref(id) =>
          schema.find(_.name == id) match
            case Some(column) => Ref(column)
            case None         => throw Exception("Column not found")
        case ValueExpr.Int(value) => Int(value)
        case ValueExpr.Str(value) => Str(value)
    rec(expr)

  def name(typed: TypedExpr): String =
    typed match
      case Cast(expr, to)    => s"cast(${name(expr)}, ${Type.fmt(to)})"
      case RowID(relation)   => s"${relation.name}.#"
      case Ref(column)       => column.name
      case Int(value)        => s"int($value)"
      case Str(value)        => s"str($value)"
      case Bool(value)       => s"bool($value)"
      case Greater(lhs, rhs) => s"gt(${name(lhs)},${name(rhs)})"
      case Lower(lhs, rhs)   => s"lt(${name(lhs)},${name(rhs)})"
      case Equal(lhs, rhs)   => s"eq(${name(lhs)},${name(rhs)})"
      case And(lhs, rhs)     => s"and(${name(lhs)},${name(rhs)})"
      case Or(lhs, rhs)      => s"or(${name(lhs)},${name(rhs)})"
      case Not(expr)         => s"not(${name(expr)}"
