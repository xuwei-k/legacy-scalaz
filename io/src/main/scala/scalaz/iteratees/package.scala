package scalaz

import Scalaz._

/** A module for the generic implementation of Iteratees */
package object iteratees {

  type Error = String // TODO -Better error type.

  /** An enumerator that just pushes an EOF to the Iteratee */
  def enumEof[C, M[_]] = new Enumerator[C,M] {
    def apply[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]] =
      i.fold(
         cont = (f) => m.pure(f(EOF[C](None))),
         done = (value, input) => m.pure(Done(value,input)),
         error = (msg) => m.pure(Failure(msg))
      )
  }

  def enumInput[C, M[_]](in : Input[C]) = new Enumerator[C,M] {
    def apply[A](i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]] =
      i.fold(
        cont = (f) => m.pure(f(in)),
        done = (value, input) => m.pure(Done(value, input)),
        error = (msg) => m.pure(Failure(msg))
      )
  }

  def readLength[C,M[_]]( sizeOfChunk : C => Long)(implicit m : Monad[M]) : Iteratee[C,M,Long] = {
    def step(curSize : Long)(in : Input[C]) : Iteratee[C,M,Long] = in match {
      case Chunk(c) => Cont( i => step(curSize + sizeOfChunk(c))(i))
      case EOF(Some(err)) => Failure(err)
      case EOF(None) => Done(curSize, in)
    }
    Cont(i => step(0)(i))
  }


}