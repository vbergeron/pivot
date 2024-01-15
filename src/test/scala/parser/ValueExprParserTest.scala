package pivot
package parser

import munit.FunSuite
import pivot.parser.ParseTree.ValueExpr.*

class ValueExprParserTest extends FunSuite, ParserTest {
  test("expr: dot") {
    parseSuccess(ValueExprParser.dot)(".", Dot)
  }

  test("expr: hash") {
    parseSuccess(ValueExprParser.hash)("#", Hash)
  }

  test("expr: str") {
    parseSuccess(ValueExprParser.str)("'foo bar based'", Str("foo bar based"))
  }

  test("expr: int") {
    parseSuccess(ValueExprParser.int)("123123", Int(123123))
  }

  test("expr: ref") {
    parseSuccess(ValueExprParser.ref)("name", Ref("name"))
  }

  test("expr: query") {
    parseSuccess(ValueExprParser.query)(
      "run Dept[#, name = 'it']",
      Query(force(QueryParser.apply)("Dept[#, name = 'it']"))
    )
  }

  test("expr: lt") {
    parseSuccess(ValueExprParser.apply)(
      "1 < 2",
      Lower(Int(1), Int(2))
    )
  }

  test("expr: gt") {
    parseSuccess(ValueExprParser.apply)(
      "2 > 1",
      Greater(Int(2), Int(1))
    )
  }

  test("expr: eq") {
    parseSuccess(ValueExprParser.apply)(
      "name = 1",
      Equal(Ref("name"), Int(1))
    )
  }

  test("expr: and") {
    parseSuccess(ValueExprParser.apply)(
      "name = 'foo' & age > 18",
      And(Equal(Ref("name"), Str("foo")), Greater(Ref("age"), Int(18)))
    )
  }

  test("expr: or") {
    parseSuccess(ValueExprParser.apply)(
      "name = 'foo' | age > 18",
      Or(Equal(Ref("name"), Str("foo")), Greater(Ref("age"), Int(18)))
    )
  }

  test("expr: predicate with query") {
    parseSuccess(ValueExprParser.apply)(
      "salary < run Dept[maxSalary, name = 'it']",
      Lower(
        Ref("salary"),
        Query(force(QueryParser.apply)("Dept[maxSalary, name = 'it']"))
      )
    )
  }

}
