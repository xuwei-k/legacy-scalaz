package scalaz.iteratees

import scalaz._
import Scalaz._

/** An enumerator is something that takes an iteratee and drives
 * input through it.
 */
trait Enumerator[C, M[_]] { self =>
  def apply[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]]

  /**Operator form of apply.  This is useful when the Iteratee type is known and is driven by a generic
   * enumerator.
   */
  def <<:[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) = apply[A](i)(m)

  /** Combinator for Enumerators.   Will run through the data in this enumerator and then the data in the
   *  chained enumerator.
   */
  def andThen(e : Enumerator[C,M]) : Enumerator[C,M] = new Enumerator[C,M] {
    def apply[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]] =
      self(i).flatMap(e.apply)
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
trait Enumeratee[CFrom,CTo, M[_],A] {
  def apply(i : Iteratee[CTo,M,A])(implicit m : Monad[M]) : Iteratee[CFrom, M, Iteratee[CTo,M,A]]
}



trait Nullable[S] {
  def isNull(s : S) : Boolean
}