package scalaz
package parse
package plain

import base._
import Parser._
import Result._
import Commitment._

object ParserTest {
  val p3 = for {
    x <- next[Char]
    y <- next[Char]
    z <- next[Char]
  } yield (x :: y :: z :: Nil).mkString

  val p2 = for {
    x <- next[Char]
    y <- next[Char]
  } yield (x :: y :: Nil).mkString

  type P[A] = Parser[Char, A]

  val p3or2 = oneOf[String, P](List(p3, p2))
  val m = manyFinally[String, String, P](p2, p3)

  def run(s: String) = runParser(p2, s.toList)
}
