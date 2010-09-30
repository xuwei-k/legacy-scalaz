package scalaz

sealed trait LongW extends PimpedType[Long] {
  import Multiplication._
  import LongW._

  def ∏ : LongMultiplication = multiplication(value)

  def digits: List[Digit] =
    if (value == 0) List(0L)
    else if (value < 10) List(value)
    else value % 10L :: (value / 10L digits)
}

trait Longs {
  implicit def LongTo(n: Long): LongW = new LongW {
    val value = n
  }
}

object LongW extends Longs