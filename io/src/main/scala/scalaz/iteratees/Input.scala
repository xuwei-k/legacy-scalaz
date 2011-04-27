package scalaz.iteratees
import scalaz._
import Scalaz._

/**
 * A continuing stream of 'chunked' elements.  A chunk is a bundle of data, Represented by the type C.
 * EOF is an end-of-sequence signal that represents either end of input, or a processing error occurred.
 */
sealed trait Input[C]

object Input {
  /** Instance of Monoid for Input that requires C to be a monoid. */
  implicit def inputMonoid[C](implicit m : Monoid[C]) = new Monoid[Input[C]] {
    override def append(s1: Input[C], s2: => Input[C]) : Input[C] =  (s1, s2) match {
      case (Chunk(c0), Chunk(c1)) => Chunk(m.append(c0,c1))
      case (EOF(err), EOF(err2)) => EOF(err orElse err2)
      case (i @ EOF(_), _) => i
      case (_, i @ EOF(_)) => i
    }
    override val zero : Input[C] = Chunk(m.zero)
  }
  /** An instance of Functor for Input */
  implicit def functor = new Functor[Input] {
    override def fmap[A,B](r: Input[A], f: (A) => B) : Input[B] = r match {
      case Chunk(c) => Chunk(f(c))
      case e @ EOF(err) => EOF(err)
    }
  }
}

/**
 * This class represents a 'chunk' of input with the type C.   C should be an instance of TBD type class.
 */
case class Chunk[C](c : C) extends Input[C]

/**
 * This class represents the EOF signal.   The EOF signal can represent an error by providing the optional
 * Error value.
 */
case class EOF[C](err : Option[Error]) extends Input[C]

