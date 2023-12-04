package pivot

import parser.*

import fastparse.*

object prints:

  println(parse("id", identifier(_)))
  println(parse("name", QueryParser.subclause(_)))
  println(parse("foo := bar", QueryParser.subclause(_)))
  println(parse("name ^", QueryParser.subclause(_)))
  println(parse("name v", QueryParser.subclause(_)))
  println(parse("age > 18", QueryParser.subclause(_)))
  println(parse("[name]", QueryParser.clause(_)))
  println(parse("[name, age ^, salary v]", QueryParser.clause(_)))
  println(
    parse(
      "Employees [name, age, salary, salary > 20000][age ^, salary v]",
      QueryParser.apply(_)
    )
  )

  println(parse("int", SchemaParser._type(_)))
  println(parse("str", SchemaParser._type(_)))
  println(parse("flt", SchemaParser._type(_)))

  println(
    parse(
      """rel Employee {
        |  name: str,
        |  age: int [. < 18],
        |  Dept.#
        |}""".stripMargin,
      SchemaParser.apply(_)
    )
  )

  println(parse("{name: 'foo', age: 28}", SetExprParser.apply(_)))
  println(parse("Employees + Employees", SetExprParser.apply(_)))
  println(
    parse(
      """Employee
        |  + {name: 'foo', age: 18}
        |  + {name: 'bar', age: 52}
        |""".stripMargin,
      SetExprParser.apply(_)
    )
  )
  println(
    parse(
      """Employee - {name: 'foo', age: 18} + {name: 'bar', age: 52}
        |""".stripMargin,
      SetExprParser.apply(_)
    )
  )
  println(
    parse(
      "Employee > stdout",
      SetExprParser.apply(_)
    )
  )

  println(parse("'foo bar barred'", ValueExprParser.apply(_)))
  println(parse("123456", ValueExprParser.apply(_)))

  println(parse("foo < 2", ValueExprParser.apply(_)))
