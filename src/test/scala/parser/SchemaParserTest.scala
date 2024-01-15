package pivot
package parser

import parser.ParseTree.*

import munit.FunSuite

class SchemaParserTest extends FunSuite, ParserTest {

  test("type: Int") {
    parseSuccess(SchemaParser._type)("int", Type.Int)
  }
  test("type: Str") {
    parseSuccess(SchemaParser._type)("str", Type.Str)
  }
  test("type: Flt") {
    parseSuccess(SchemaParser._type)("flt", Type.Flt)
  }

  test("column: value") {
    parseSuccess(SchemaParser.column)(
      "name: str",
      Column.Value("name", Type.Str, None)
    )
  }

  test("column: constrained value") {
    parseSuccess(SchemaParser.column)(
      "age: int [. > 0]",
      Column.Value(
        "age",
        Type.Int,
        Some(force(ValueExprParser.predicate)(". > 0"))
      )
    )
  }

  test("column: ref") {
    parseSuccess(SchemaParser.column)(
      "Employee.#",
      Column.Ref("Employee")
    )
  }

  test("schema") {
    parseSuccess(SchemaParser.apply)(
      "rel Employee { name: str, age: int [. > 0], Dept.# }",
      Relation(
        "Employee",
        Seq(
          Column.Value("name", Type.Str, None),
          Column.Value(
            "age",
            Type.Int,
            Some(force(ValueExprParser.predicate)(". > 0"))
          ),
          Column.Ref("Dept")
        )
      )
    )
  }
}
