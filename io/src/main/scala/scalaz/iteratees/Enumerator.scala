package scalaz.iteratees

import scalaz._
import Scalaz._

/** An enumerator is something that takes an iteratee and drives
 * input through it.
 */
trait Enumerator[C, M[_]] {
  def apply[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]]
}
/** An Enumeratee is something that both reads and writes to a location. */
trait Enumeratee[CFrom,CTo, M[_]] {
  def apply[A](i : Iteratee[CTo,M,A]) : Iteratee[CFrom, M, Iteratee[CTo,M,A]]
}



trait Nullable[S] {
  def isNull(s : S) : Boolean
}
trait EmptyChunk[C] {
  def empty : C
}