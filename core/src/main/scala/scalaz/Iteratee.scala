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
    def fold[R](done : (=> A,  => Input[C]) => R,
                cont : ((=> Input[C]) => Iteratee[C,M,A]) => R,
                error : (=> Error) => R
                ) : R

    def mapIteratee[N[_]](f : M[A] => N[B])(implicit m : Monad[M], n : Monad[N], s : NullPoint[C]) : Iteratee[C,N,B] = error("todo")
    def run(implicit m : Monad[M]) : M[A] = error("todo")
  }
  // TODO -

  object Done {
    def apply[C,M[_], A]( a : => A, input : => Input[C]) = new Iteratee[C,M,A] {
      def fold[R](done : (=> A,  => Input[C]) => R,
                  cont : ((=> Input[C]) => Iteratee[C,M,A]) => R,
                  error : (=> Error) => R
                  ) : R = done(a, input)
    }
    def unapply[C,M[_], A](i : Iteratee[C,M,A]) : Option[(A, Input[C])] =
      i.fold(done = { (value, lastInput) => Some((value, lastInput)) },
             cont = ignore =>  None,
             error = ignore => None)
  }

  object Cont {
    def apply[C, M[_],A](f : (=> Input[C]) => Iteratee[C,M,A]) = new Iteratee[C,M,A] {
      def fold[R](done : (=> A,  => Input[C]) => R,
                  cont : ((=> Input[C]) => Iteratee[C,M,A]) => R,
                  error : (=> Error) => R
                  ) : R = cont(f)
    }
    def unapply[C,M[_], A](i : Iteratee[C,M,A]) : Option[(=> Input[C]) => Iteratee[C,M,A]] =
      i.fold(done = (_,_) => None,
             cont = Some(_),
             error = _ => None)
  }
  object Failure {
    def apply[C, M[_], A](err : => Error) = new Iteratee[C,M,A] {
      def fold[R](done : (=> A,  => Input[C]) => R,
                  cont : ((=> Input[C]) => Iteratee[C,M,A]) => R,
                  error : (=> Error) => R
                  ) : R = error(err)
    }
    def unapply[C, M[_], A](i : Iteratee[C,M,A]) : Option[Error] =
      i.fold(done = (_,_) => None,
             cont = _ => None,
             error = Some(_))
  }

  trait Nullable[S] {
    def isNull(s : S) : Boolean
  }
}

// TODO - Better moduling?
object Iteratees extends Iteratees
