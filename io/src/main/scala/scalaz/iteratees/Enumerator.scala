package scalaz.iteratees

import scalaz._
import Scalaz._

/** An enumerator is something that takes an iteratee and drives
 * input through it.
 */
trait Enumerator[C, M[_]] {
  def apply[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]]

  /** Combinator for Enumerators.   Will run through the data in this enumerator and then the data in the
   *  chained enumerator.
   */
  def andThen(e : Enumerator[C,M]) : Enumerator[C,M] = new Enumerator[C,M] {
    def apply[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]] =
      this(i).flatMap(e.apply)
  }
}

/** An Enumeratee is something that both reads and writes from a location.
 *  Enumeratee's are classically used to 'vertically' compose streams.   That is, construct a higher level
 *  stream from a lower-level stream.
 *
 *  Because a low level stream my be exhausted/fail before the higher-level stream, they are both returned.
 *  That is, the underlying stream (Iteratee[CFrom, M, _]) for the Enumeratee is exposed in the resulting value from
 *  'driving' the higher level stream (Iteratee[CTo,M,A])
 */
trait Enumeratee[CFrom,CTo, M[_]] {
  def apply[A](i : Iteratee[CTo,M,A]) : Iteratee[CFrom, M, Iteratee[CTo,M,A]]
}



trait Nullable[S] {
  def isNull(s : S) : Boolean
}
trait EmptyChunk[C] {
  def empty : C
}