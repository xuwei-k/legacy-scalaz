package scalaz.iteratees

import org.specs.{Sugar, Specification}
import scalaz._
import Scalaz._

class IterateeTraitTest extends Specification with Sugar {
  "Iteratees" should {
    "ignore eof when done" in {
      val x = 5.pure[({type I[X]=Iteratee[Seq[Int], Identity, X]})#I]
      val y = enumEof(x)
      y.run must beEqual(5)
    }
  }
}