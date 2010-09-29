package scalaz

import java.math.BigInteger

sealed trait BigIntegerW extends PimpedType[BigInteger] {
  import Multiplication._

  def ∏ = multiplication(value)
}

trait BigIntegers {
  implicit def BigIntegerTo(n: BigInteger): BigIntegerW = new BigIntegerW {
    val value = n
  }
}

object BigIntegerW extends BigIntegers
