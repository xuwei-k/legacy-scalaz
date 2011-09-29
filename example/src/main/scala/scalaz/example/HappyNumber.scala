package scalaz.example

import scalaz._
import Scalaz._

object HappyNumber {
  type StS =
    {type λ[α] = State[Set[Long], α]}

  val happy =
    ((_: EStream[Long]).findM[StS#λ](j =>
        state(s => (j == 1 || (s contains j), s + j))) eval Set() element 1) compose (iterate(_: Long)(k =>
            (k.toString map (((x: Long) => x * x) compose ((_: Char).toLong - 48))) sum))

  def main(args: Array[String]) {
    // [true,false,true,false,false,true,true,true,true,true,true,false]
    List(44L, 51, 193, 194, 204, 203, 208, 356, 362, 365, 487, 488) map happy println
  }
}