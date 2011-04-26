package scalaz

import Scalaz._
/** A module for the generic implementation of Iteratees */
trait Iteratees {

  type Error = String // TODO -Better error type.
  /**
   * A continuing sequence of 'chunked' elements.  A chunk is a bundle of data, Represented by the type C.
   * EOF is an end-of-sequence signal that represents either end of input, or a processing error occurred.
   */
  sealed trait Input[C]
  // TODO - Monoid, Functor
  case class Chunk[C](c : C) extends Input[C]
  case class EOF[C](err : Option[Error]) extends Input[C]


  sealed trait Iteratee[C,M[_],A] {
    def fold[R](done : (=> A,  => Input[C]) => M[R],
                cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                error : (=> Error) => M[R]
                ) : M[R]

    def mapIteratee[N[_],B](f : M[A] => N[B])(implicit m : Monad[M], n : Monad[N], s : EmptyChunk[C]) : Iteratee[C,N,B] = error("todo")
    def run(implicit m : Monad[M]) : M[A] = {
      enumEof[C,M,A](this).flatMap(_.fold(
        done = (value, _) => m.pure(value),
        error = (msg) => m.pure(error(msg)),
        cont  = (f) => m.pure(error("Divergent Iteratee!"))
      ))
    }

    /*def apply(chunk : => Input[C]) : Iteratee[C,M,A] =
      fold(done = (value, input) => Done(value, input),
           error = (msg) => Failure(msg),
           cont = (f) => f(chunk))*/
  }
  // TODO -

  object Done {
    def apply[C,M[_], A]( a : => A, input : => Input[C])(implicit m : Monad[M]) = new Iteratee[C,M,A] {
      def fold[R](done : (=> A,  => Input[C]) => M[R],
                  cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                  error : (=> Error) => M[R]
                  ) : M[R] = done(a, input)
    }
  }

  object Cont {
    def apply[C, M[_],A](f : (=> Input[C]) => Iteratee[C,M,A]) = new Iteratee[C,M,A] {
      def fold[R](done : (=> A,  => Input[C]) => M[R],
                  cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                  error : (=> Error) => M[R]
                  ) : M[R] = cont(f)
    }
  }
  object Failure {
    def apply[C, M[_], A](err : => Error) = new Iteratee[C,M,A] {
      def fold[R](done : (=> A,  => Input[C]) => M[R],
                  cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                  error : (=> Error) => M[R]
                  ) : M[R] = error(err)
    }
  }
  /** An enumerator is something that takes an iteratee and drives
   * input thorugh it.
   */
  trait Enumerator[C, M[_], A] {
     def apply(i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]]
  }

  /** An enumerator that just pushes an EOF to the Iteratee */
  def enumEof[C, M[_], A] = new Enumerator[C,M,A] {
    def apply(i : Iteratee[C,M,A])(implicit m : Monad[M]) : M[Iteratee[C,M,A]] =
      i.fold(
         cont = (f) => m.pure(f(EOF[C](None))),
         done = (value, input) => m.pure(Done(value,input)),
         error = (msg) => m.pure(Failure(msg))
      )
  }
  /** An enumeratee is something that both reads and writes to a location. */
  trait Enumeratee[CFrom,CTo, M[_], A] {
    def apply(i : Iteratee[CTo,M,A]) : Iteratee[CFrom, M, Iteratee[CTo,M,A]]
  }

  

  trait Nullable[S] {
    def isNull(s : S) : Boolean
  }
  trait EmptyChunk[C] {
    def empty : C
  }
}

// TODO - Better moduling?
object Iteratees extends Iteratees
