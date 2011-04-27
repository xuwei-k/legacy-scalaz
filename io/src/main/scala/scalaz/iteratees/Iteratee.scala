package scalaz.iteratees

import scalaz._
import Scalaz._

sealed trait Iteratee[C,M[_],A] {
  /** Fold over the current possible states of this iteratee. */
  def fold[R](done : (=> A,  => Input[C]) => M[R],
              cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
              error : (=> Error) => M[R]
              ) : M[R]
  def mapIteratee[N[_],B](f : M[A] => N[B])(implicit m : Monad[M], n : Monad[N], s : EmptyChunk[C]) : Iteratee[C,N,B] = error("todo")
  /** Sends an EOF to the stream and tries to extract the value.  Note: this can pass errors into the Monad */
  def run(implicit m : Monad[M]) : M[A] = {
    enumEof[C,M](this).flatMap(_.fold(
      done = (value, _) => m.pure(value),
      error = (msg) => m.pure(error(msg)),
      cont  = (f) => m.pure(error("Divergent Iteratee!"))
    ))
  }
}

object Iteratee {
  /** Instance of Pure for Iteratees */
  implicit def iterateePure[C, M[_]](implicit m : Monad[M]) = new Pure[({type I[A] = Iteratee[C,M,A]})#I] {
    override def pure[A](a : => A) : Iteratee[C,M,A] = Done(a, EOF(None))
  }
  implicit def iterateeBind[C,M[_]](implicit m : Monad[M]) =
    new Bind[({type I[A] = Iteratee[C,M,A]})#I] {
      override def bind[A,B](i : Iteratee[C,M,A], f : A => Iteratee[C,M,B]) : Iteratee[C,M,B] = {
        FlattenI(
          i.fold[Iteratee[C,M,B]](
            done = (value, input) =>
              enumInput(input)(f(value)), // Pass the remaining value to the Iteratee.
            cont = (k) => m.pure(Cont(in => bind(k(in), f))),
            error = (msg) => m.pure(Failure(msg))
          )
        )
      }
    }
}

/**
 * Constructs/matches Iteratees that are in a 'finished' state.
 */
object Done {
  def apply[C,M[_], A]( a : => A, input : => Input[C])(implicit m : Monad[M]) = new Iteratee[C,M,A] {
    def fold[R](done : (=> A,  => Input[C]) => M[R],
                cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                error : (=> Error) => M[R]
                ) : M[R] = done(a, input)
  }
}

/**
 * Constructs/matches Iteratees that are in a 'in progress' or 'continuation' state.
 */
object Cont {
  def apply[C, M[_],A](f : (=> Input[C]) => Iteratee[C,M,A]) = new Iteratee[C,M,A] {
    def fold[R](done : (=> A,  => Input[C]) => M[R],
                cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                error : (=> Error) => M[R]
                ) : M[R] = cont(f)
  }
}

/**
 * Constructs/matches Iteratees that are in a 'failed' state.
 */
object Failure {
  def apply[C, M[_], A](err : => Error) = new Iteratee[C,M,A] {
    def fold[R](done : (=> A,  => Input[C]) => M[R],
                cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                error : (=> Error) => M[R]
                ) : M[R] = error(err)
  }
}


  /**Flattens an Iteratee inside the Monad M to just be an Iteratee.
   * This works because Iteratees are threaded through the Monad M.
   */
  object FlattenI {
    def apply[C, M[_], A](i : M[Iteratee[C,M,A]])(implicit m : Monad[M]) = new Iteratee[C,M,A] {
      def fold[R](done : (=> A,  => Input[C]) => M[R],
                  cont : ((=> Input[C]) => Iteratee[C,M,A]) => M[R],
                  error : (=> Error) => M[R]
                  ) : M[R] =
        i.flatMap(x => x.fold(done = done, cont = cont, error = error))
    }
  }