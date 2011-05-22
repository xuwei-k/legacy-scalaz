package scalaz

import Scalaz._

/** A module for the generic implementation of Iteratees */
package object iteratees {

  type Error = String // TODO -Better error type.

  /** An enumerator that just pushes an EOF to the Iteratee */
  def enumEof[C, M[_]] : Enumerator[C,M] = enumInput(EOF(None))

  def enumInput[C, M[_]](in : Input[C]) : Enumerator[C,M] = new Enumerator[C,M] {
    def apply[A](i: Iteratee[C,M,A])(implicit m : Monad[M]): M[Iteratee[C,M,A]] = {
      i.fold(
        cont = (f) => f(in).pure,
        done = (value, input) => i.pure,
        error = (msg, input) => i.pure
      )
    }
  }

  /** Pulls the length from an enumeratee */
  // TODO - We would use Length here, but it requires a higher-kinded type and defaults to 1 for non-higherkinded
  // types, and therefore does *not* work for java.nio.ByteBuffer...
  def readLength[C ,M[_] : Monad]( sizeOfChunk : C => Long): Iteratee[C,M,Long] = {
    def step(curSize: Long)(in: Input[C]): Iteratee[C,M,Long] = in match {
      case Chunk(c) => Cont(step(curSize + sizeOfChunk(c)))
      case EOF(Some(err)) => Failure(err, EOF(Some(err)))
      case EOF(None) => Done(curSize, in)
    }
    Cont(step(0))
  }

  /** Seeks to a specific index in the input, buffered by a Rope. */
  def seek[M[_]: Monad, A:Manifest](i: Int): Iteratee[Rope[A], M, A] = {
    def step: Input[Rope[A]] => Iteratee[Rope[A], M, A] = {
      case a@Chunk(h) => if (i < h.size)
                           Done(h(i), EOF(none))
                         else Cont(b => step((a:Input[Rope[A]]) |+| b))
      case EOF(e) => Failure(e getOrElse "EOF", EOF(e))
    }
    Cont(step)
  }
  // TODO - Use typeclass to ignore 'zero' inputs.
  def head[M[_] : Monad, C]: Iteratee[C, M, Option[C]] = {
    def step : Input[C] => Iteratee[C,M,Option[C]] = {
      case i@Chunk(h) => Done(Some(h), i)
      case EOF(None) => Done(None, EOF(None))
      case EOF(Some(err)) => Failure(err, EOF(Some(err)))
    }
    Cont(step)
  }

  def joinI[CFrom : EmptyChunk, CTo, M[_]: Monad, A](iter: Iteratee[CFrom, M, Iteratee[CTo, M, A]]): Iteratee[CFrom, M, A] = {
    iter flatMap { iter2 => FlattenI(iter2.fold(
      done = (a, i) => Done(a, EmptyChunk[CFrom]).pure,
      error = (e,_) => Failure(e, EmptyChunk[CFrom]).pure,
      cont = k => k(EOF(None)).fold(
        done = (a, i) => Done(a, EmptyChunk[CFrom]).pure,
        error = (e, _) => Failure(e, EmptyChunk[CFrom]).pure,
        cont = k => error("joinI: Divergent iteratee!")
      )
    ))}
  }
}
