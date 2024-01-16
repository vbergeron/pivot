package pivot
package parser

import ParseTree.*
import ParseTree.SetExpr.*

class SetExprParserTest extends munit.FunSuite, ParserTest {
  test("setexpr: record"):
    parseSuccess(SetExprParser.record)(
      "{'foo', some: 'bar', 123, name: run Dept[#]}",
      Record(
        Seq(
          Field(None, ValueExpr.Str("foo")),
          Field(Some("some"), ValueExpr.Str("bar")),
          Field(None, ValueExpr.Int(123)),
          Field(Some("name"), force(ValueExprParser.apply)("run Dept[#]"))
        )
      )
    )

  // TODO: Somehow, anonymous subqueries won't parse
  test("setexpr: record with anonymous subquery".ignore):
    parseSuccess(SetExprParser.record)(
      "{run Dept[#]}",
      Record(
        Seq(
          Field(None, force(ValueExprParser.apply)("run Dept[#]"))
        )
      )
    )

  test("setexpr: union"):
    parseSuccess(SetExprParser.apply)(
      "A[#] + B[#] + {1} + {2} + {3}",
      Union(
        QueryWrap(force(QueryParser.apply)("A[#]")),
        Seq(
          QueryWrap(force(QueryParser.apply)("B[#]")),
          Record(Seq(Field(None, ValueExpr.Int(1)))),
          Record(Seq(Field(None, ValueExpr.Int(2)))),
          Record(Seq(Field(None, ValueExpr.Int(3))))
        )
      )
    )

  test("setexpr: diff"):
    parseSuccess(SetExprParser.apply)(
      "A[#] - B[#] - {1} - {2} - {3}",
      Diff(
        QueryWrap(force(QueryParser.apply)("A[#]")),
        Seq(
          QueryWrap(force(QueryParser.apply)("B[#]")),
          Record(Seq(Field(None, ValueExpr.Int(1)))),
          Record(Seq(Field(None, ValueExpr.Int(2)))),
          Record(Seq(Field(None, ValueExpr.Int(3))))
        )
      )
    )
  test("setexpr: diff of union"):
    parseSuccess(SetExprParser.apply)(
      "(A[#] + B[#]) - ({1} + {2} + {3})",
      Diff(
        Union(
          QueryWrap(force(QueryParser.apply)("A[#]")),
          Seq(QueryWrap(force(QueryParser.apply)("B[#]")))
        ),
        Seq(
          Union(
            Record(Seq(Field(None, ValueExpr.Int(1)))),
            Seq(
              Record(Seq(Field(None, ValueExpr.Int(2)))),
              Record(Seq(Field(None, ValueExpr.Int(3))))
            )
          )
        )
      )
    )

  test("setexpr: pipe to"):
    parseSuccess(SetExprParser.apply)(
      "(A[#] + B[#]) > stdout",
      PipeTo(force(SetExprParser.apply)("A[#] + B[#]"), "stdout")
    )

  test("setexpr: inner join"):
    parseSuccess(SetExprParser.apply)(
      "A[#] <> B[#]",
      Join(
        QueryWrap(force(QueryParser.apply)("A[#]")),
        QueryWrap(force(QueryParser.apply)("B[#]")),
        JoinType.Inner
      )
    )

  test("setexpr: right join"):
    parseSuccess(SetExprParser.apply)(
      "A[#] >> B[#]",
      Join(
        QueryWrap(force(QueryParser.apply)("A[#]")),
        QueryWrap(force(QueryParser.apply)("B[#]")),
        JoinType.Right
      )
    )

  test("setexpr: left join"):
    parseSuccess(SetExprParser.apply)(
      "A[#] << B[#]",
      Join(
        QueryWrap(force(QueryParser.apply)("A[#]")),
        QueryWrap(force(QueryParser.apply)("B[#]")),
        JoinType.Left
      )
    )

  test("setexpr: full join"):
    parseSuccess(SetExprParser.apply)(
      "A[#] >< B[#]",
      Join(
        QueryWrap(force(QueryParser.apply)("A[#]")),
        QueryWrap(force(QueryParser.apply)("B[#]")),
        JoinType.Full
      )
    )

}
