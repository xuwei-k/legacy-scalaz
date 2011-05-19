package scalaz.iteratees

import org.specs.{Sugar, Specification}
import scalaz._
import Scalaz._

class IterateeTraitTest extends Specification with Sugar {
  "Iteratees" should {
    "ignore eof when done" in {
      val x = 5.pure[({type I[X]=Iteratee[Seq[Int], Identity, X]})#I]
      val y = enumEof(x)
      val result : Int = y.run
      result must beEqualTo(5)
    }
    "coiterate on zip" in {
      def sizer = readLength[Seq[Int], Identity](_.size)
      val cosizers = sizer zip sizer
      val result : (Long, Long) = enumInput(Chunk(Seq(1,2)))(cosizers).run
      result must beEqualTo((2,2))
    }
  }
}