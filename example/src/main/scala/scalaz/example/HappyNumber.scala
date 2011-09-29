package scalaz.example

import scalaz._
import Scalaz._

object HappyNumber {
  type StS[A] =
    State[Set[Long], A]

  val square = ((_: Long) * (_: Long)).curried.μ

  val happy =
    ((_: EStream[Long]).findM[StS](j =>
        state(s => (j == 1 || (s contains j), s + j))) eval Set() element 1) compose (iterate(_: Long)(k =>
            (k.toString map (c => square((c: Char).toLong - 48))) sum))

  def main(args: Array[String]) {
    // [true,false,true,false,false,true,true,true,true,true,true,false]
    List(44L, 51, 193, 194, 204, 203, 208, 356, 362, 365, 487, 488) map happy println
  }
}

/*
import Control.Monad.State
import Control.Monad.TM
import Data.Char
import qualified Data.Set as S
import qualified Data.Foldable as F

happy ::
  Integer
  -> Bool
happy =
  F.elem 1 .
  (`evalState` S.empty) .
  findM (\j -> state $ \s -> (j == 1 || S.member j s, S.insert j s)) .
  iterate (sum .
           map (join (*) .
                toInteger .
                digitToInt) .
           show)
*/
