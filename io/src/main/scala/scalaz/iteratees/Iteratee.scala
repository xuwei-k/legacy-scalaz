package scalaz.iteratees

import scalaz._
import Scalaz._

/**
 * A generic stream processor.  An Iteratee takes 'chunked' input of type C, processes it inside the monad M and
 * returns a result of type A.
 *
 * Iteratees have three potential states:
 * - cont => Requires more data
 * - done => Complete with remaining Chunked input.
 * - error => Encountered some form of error during processing.
 *
 * Note: Iteratees are threaded through the Monad M.  For simple 'strict' iterators, or for testing, it's useful
 * to use Id for M.
 */
sealed abstract class Iteratee[C,M[_],A](implicit m: Monad[M]) {
  /**Fold over the current possible states of this iteratee.
   * @param done  This function will be called if the Iteratee is in a done state.  It accepts two parameters,
   *              the resulting value of this iteratee and the remaining input chunk from processing.
   * @param cont This function will be called if the Iteratee needs more input.   The continuation takes
   *               more input and returns the resulting iteratee.
   * @param error This function is called with the error if there was any during the processing.
   */
  def fold[R](done: (A, Input[C]) => M[R],
              cont: (Input[C] => Iteratee[C,M,A]) => M[R],
              error: (Error, Input[C]) => M[R]
              ): M[R]
  def mapIteratee[N[_],B](f: M[A] => N[B])(implicit n : Monad[N], s : EmptyChunk[C]) : Iteratee[C,N,B] = error("todo")

  /**Sends an EOF to the stream and tries to extract the value.
   * Note: this can pass errors into the Monad.   If the monad is strict, this can explode.
   */
  def run: M[A] = {
    enumEof[C,M](this).flatMap(_.fold(
      done = (value, _) => value.pure,
      error = (msg, input) => m.pure(error(msg)),
      cont  = (f) => m.pure(error("Divergent Iteratee!"))
    ))
  }

  def flatMap[B](f: A => Iteratee[C, M, B]): Iteratee[C, M, B] =
    Iteratee.iterateeBind[C, M].bind(this, f)

  def map[B](f: A => B): Iteratee[C, M, B] = 
    flatMap(x => Iteratee.iterateePure[C, M].pure(f(x)))

  /** If this iteratee fails, try the given iteratee instead. */
  def |(or: Iteratee[C, M, A]): Iteratee[C, M, A] =
    FlattenI(fold((_, _) => this.pure[M], _ => this.pure[M], (e, i) => or.pure[M]))
}

object Iteratee {
  /** Instance of Pure for Iteratees */
  implicit def iterateePure[C, M[_]](implicit m: Monad[M]) = new Pure[({type I[A] = Iteratee[C,M,A]})#I] {
    override def pure[A](a : => A) : Iteratee[C,M,A] = Done(a, EOF(None))
  }
  implicit def iterateeBind[C,M[_] : Monad] =
    new Bind[({type I[A] = Iteratee[C,M,A]})#I] {
      override def bind[A,B](i: Iteratee[C,M,A], f: A => Iteratee[C,M,B]) : Iteratee[C,M,B] = {
        FlattenI(
          i.fold[Iteratee[C,M,B]](
            done = (value, input) =>
              enumInput(input)(f(value)), // Pass the remaining value to the Iteratee.
            cont = (k) => Cont[C,M,B](in => bind(k(in),f)).pure,
            error = (msg, input) => Failure(msg, input).pure
          )
        )
      }
    }
  implicit def iterateeFunctor[C, M[_]: Monad]: Functor[({type λ[α]=Iteratee[C,M,α]})#λ] =
    new Functor[({type λ[α]=Iteratee[C,M,α]})#λ] {
      def fmap[A, B](r: Iteratee[C, M, A], f: A => B) = r map f
    }

  // Crashes the compiler
  // implicit def iterateeMonad[C, M[_]: Monad]: Monad[({type λ[α]=Iteratee[C,M,α]})#λ] =
  //   Monad.monad[({type λ[α]=Iteratee[C,M,α]})#λ](iterateeBind[C, M], iterateePure[C, M])
}

/**
 * Constructs/matches Iteratees that are in a 'finished' state.
 */
object Done {
  def apply[C, M[_]: Monad, A](a: A, input:  Input[C]) = new Iteratee[C,M,A] {
    def fold[R](done: (A,  Input[C]) => M[R],
                cont: (Input[C] => Iteratee[C,M,A]) => M[R],
                error: (Error, Input[C]) => M[R]
                ): M[R] = done(a, input)
  }
}

/**
 * Constructs/matches Iteratees that are in a 'in progress' or 'continuation' state.
 */
object Cont {
  def apply[C, M[_]: Monad, A](f : (Input[C]) => Iteratee[C,M,A]) = new Iteratee[C,M,A] {
    def fold[R](done: (A, Input[C]) => M[R],
                cont: (Input[C] => Iteratee[C,M,A]) => M[R],
                error: (Error, Input[C]) => M[R]
                ): M[R] = cont(f)
  }
}

/**
 * Constructs/matches Iteratees that are in a 'failed' state.
 */
object Failure {
  def apply[C, M[_]: Monad, A](err: => Error, input: Input[C]) = new Iteratee[C,M,A] {
    def fold[R](done: (A, Input[C]) => M[R],
                cont: (Input[C] => Iteratee[C,M,A]) => M[R],
                error: (Error, Input[C]) => M[R]
                ): M[R] = error(err, input)
  }
}


  /**Flattens an Iteratee inside the Monad M to just be an Iteratee.
   * This works because Iteratees are threaded through the Monad M.
   */
  object FlattenI {
    def apply[C, M[_]: Monad, A](i: M[Iteratee[C,M,A]]) = new Iteratee[C,M,A] {
      def fold[R](done: (A,  Input[C]) => M[R],
                  cont: (Input[C] => Iteratee[C,M,A]) => M[R],
                  error: (Error, Input[C]) => M[R]
                  ): M[R] =
        i.flatMap(x => x.fold(done = done, cont = cont, error = error))
    }
  }