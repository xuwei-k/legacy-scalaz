package scalaz

import Scalaz._

/** A module for the generic implementation of Iteratees */
package object iteratees {

  type Error = String // TODO -Better error type.

  /** An enumerator that just pushes an EOF to the Iteratee */
  def enumEof[C, M[_]] = new Enumerator[C,M] {
    def apply[A](i: Iteratee[C,M,A])(implicit m : Monad[M]): M[Iteratee[C,M,A]] =
      i.fold(
         cont = (f) => f(EOF[C](None)).pure,
         done = (value, input) => Done(value,input).pure,
         error = (msg) => Failure(msg).pure
      )
  }

  def enumInput[C, M[_]](in : Input[C]) = new Enumerator[C,M] {
    def apply[A](i: Iteratee[C,M,A])(implicit m : Monad[M]): M[Iteratee[C,M,A]] =
      i.fold(
        cont = (f) => f(in).pure,
        done = (value, input) => Done(value, input).pure,
        error = (msg) => Failure(msg).pure
      )
  }

  /** Pulls the length from an enumeratee */
  def readLength[C,M[_] : Monad]( sizeOfChunk : C => Long): Iteratee[C,M,Long] = {
    def step(curSize: Long)(in: Input[C]): Iteratee[C,M,Long] = in match {
      case Chunk(c) => Cont(step(curSize + sizeOfChunk(c)))
      case EOF(Some(err)) => Failure(err)
      case EOF(None) => Done(curSize, in)
    }
    Cont(step(0))
  }
}