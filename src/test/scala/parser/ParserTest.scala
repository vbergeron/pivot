package pivot
package parser

import fastparse.*
import munit.BaseFunSuite

trait ParserTest extends BaseFunSuite {
  protected def parseSuccess[T](
      parser: P[?] => P[T]
  )(input: String, ref: T): Unit =
    fastparse.parse(input, parser, verboseFailures = true) match
      case success: Parsed.Success[T] =>
        assertEquals(success.value, ref)
      case failure: Parsed.Failure => fail(failure.longMsg)

  protected def force[T](parser: fastparse.P[?] => fastparse.P[T])(
      input: String
  ): T =
    fastparse.parse(input, parser, verboseFailures = true).get.value

}
