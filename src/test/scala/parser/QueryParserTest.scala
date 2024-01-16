package pivot
package parser

import munit.FunSuite
import ParseTree.SubClause.*
import ParseTree.*

class QueryParserTest extends FunSuite, ParserTest {
  test("query: sort asc"):
    parseSuccess(QueryParser.sort)("name ^", Sort("name", Order.Asc))

  test("query: sort desc"):
    parseSuccess(QueryParser.sort)("name v", Sort("name", Order.Desc))

  test("query: project"):
    parseSuccess(QueryParser.project)(
      "name",
      Project(ValueExpr.Ref("name"), None)
    )

  test("query: project with Alias"):
    parseSuccess(QueryParser.projectAlias)(
      "foobar := name",
      Project(ValueExpr.Ref("name"), Some("foobar"))
    )

  test("query: project with Alias as query"):
    parseSuccess(QueryParser.projectAlias)(
      "foobar := run Dept[#, name = 'foo']",
      Project(
        force(ValueExprParser.apply)("run Dept[#, name = 'foo']"),
        Some("foobar")
      )
    )

  test("query: filter"):
    parseSuccess(QueryParser.filter)(
      "name = 'Bob'",
      Filter(force(ValueExprParser.predicate)("name = 'Bob'"))
    )

  test("query: clause"):
    parseSuccess(QueryParser.clause)(
      "[name, name = 'Bob', id ^]",
      Query.Clause(
        Seq(
          Project(ValueExpr.Ref("name"), None),
          Filter(force(ValueExprParser.predicate)("name = 'Bob'")),
          Sort("id", Order.Asc)
        )
      )
    )

  test("query: full"):
    parseSuccess(QueryParser.apply)(
      "Employees[name, name = 'Bob', id ^][dept := run Dept[#, name = 'IT']]",
      Query(
        "Employees",
        Seq(
          Query.Clause(
            Seq(
              Project(ValueExpr.Ref("name"), None),
              Filter(force(ValueExprParser.predicate)("name = 'Bob'")),
              Sort("id", Order.Asc)
            )
          ),
          Query.Clause(
            Seq(
              force(QueryParser.subclause)("dept := run Dept[#, name = 'IT']")
            )
          )
        )
      )
    )

}
