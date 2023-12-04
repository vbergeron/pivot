package pivot

import fastparse.*
import fastparse.NoWhitespace.*

package object parser:

  def identifier[$: P]: P[String] =
    P(CharPred(_.isLetter).rep.!).filter(_.nonEmpty)

  def ws[$: P]: P[Unit] =
    CharPred(_.isWhitespace).rep
