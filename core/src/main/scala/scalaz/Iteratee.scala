package scalaz

import Scalaz._
/** A module for the generic implementation of Iteratees */
trait Iteratees {

  /**The input to an iteratee.
   * An input could have one of three types:
   *   - A chunk of of input (C[E])
   *   - End of stream signal (eof)
   *   - failure  (a string message of the failure).
   *
   *   TODO(jsuereth): Failure might also need to include a mechanism to rethread an Iteratee to
   *   this point in the stream.   M[A] => Iteratee[C,E,M,A] where M holds the stream.
   */
  sealed trait Input[C[_], E] {
    def apply[Z](chunk: (=> C[E]) => Z, eof: => Z, failure : String => Z): Z
  }

  /** A simulated case-class for a chunk of input.
   * Note: chunks can be empty.
   */
  object Chunk {
    def apply[C[_], E](e0: => C[E]): Input[C, E] = new Input[C, E] {
      def apply[Z](chunk: (=> C[E]) => Z, eof: => Z, failure : String => Z): Z = chunk(e0)
    }
    def unapply[C[_], E](r: Input[C, E]): Option[C[E]] =
      r.apply[Option[C[E]]](
        failure = _ => None,
        chunk = e => Some(e),
        eof = None)
  }
  /** A simulated case-class for a Input signal that the stream is exhausted. **/
  object EOF {
    def apply[C[_], E] : Input[C, E] = new Input[C, E] {
      def apply[Z](chunk: (=> C[E]) => Z, eof: => Z, failure : String => Z): Z = eof
    }
    def unapply[C[_], E](r: Input[C, E]): Boolean =
      r.apply[Boolean](
        failure = msg => false,
        chunk = e => false,
        eof = true)
  }

   /** A failure state from the input stream.  TODO(jsuereth): should have a seekable Iteratee to recover. **/
  object Failure {
    def apply[C[_], E](msg : => String): Input[C, E] = new Input[C, E] {
      def apply[Z](chunk: (=> C[E]) => Z, eof: => Z, failure : String => Z): Z = failure(msg)
    }

    def unapply[C[_], E](r: Input[C, E]): Option[String] =
      r.apply[Option[String]](
        failure = msg => Some(msg),
        chunk = i => None,
        eof = None)
  }

  /**Type class for dealing with stream chunks
   * TODO - Make this work well with byte buffers as Buffer[Byte]
   */
  sealed trait StreamChunk[C[_], E] {
    def length(chunk: C[E]): Int
    def empty(chunk : C[E]) : Boolean
    def head(chunk: C[E]): E
    def tail(chunk: C[E]): C[E]
    def toSeq(chunk : C[E]): Seq[E]
    def fromSeq(chunk : Seq[E]) : C[E]
    def toArray(chunk : C[E])(implicit cmf : ClassManifest[E]): Array[E]
  }

  object StreamChunk {
    // Implementation for StreamChunks of one or no value.
    implicit def optchunk[E] = new StreamChunk[Option, E] {
      def length(chunk : Option[E]) =
        chunk.map(_ => 1).getOrElse(0)
      def empty(chunk : Option[E]) = chunk.isEmpty
      def head(chunk : Option[E]) = chunk.get // unsafe
      def tail(chunk : Option[E]) = None
      def toSeq(chunk : Option[E]) = chunk.toSeq
      def fromSeq(chunk : Seq[E]) = chunk.headOption // unsafe
      def toArray(chunk : Option[E])(implicit cmf : ClassManifest[E]) =
        chunk.map(Array(_)).getOrElse(Array())
    }
    import collection.mutable.Buffer
    implicit def bufferchunk[E] = new StreamChunk[Buffer, E] {
      def length(chunk : Buffer[E]) = chunk.length
      def empty(chunk : Buffer[E]) = chunk.isEmpty
      def head(chunk : Buffer[E]) = chunk.head
      def tail(chunk : Buffer[E]) = chunk.tail
      def toSeq(chunk : Buffer[E]) = chunk.toSeq
      def fromSeq(col : Seq[E]) = col.toBuffer
      def toArray(chunk : Buffer[E])(implicit cmf : ClassManifest[E]) = chunk.toArray
    }
  }

  /**A generic stream processor.  This is what is being folded over a stream to attempt to produce a value.
   * IterGV exists in one of two states: Done or NotDone (Cont).
   *
   * The monad M defines the type of computations done by the Iteratee as it processes the stream.
   */
  sealed trait IterGV[C[_], E, M[_], A] { self =>
    // TODO(jsuereth): Add error state and 'seek' info to the fold method.
    /** Folds the given state of the stream processor to find out if it is done or needs to continue. */
    def fold[Z](done: (=> A, => Input[C, E]) => Z, cont: (=> Iteratee[C, E, M, A]) => Z): Z

    /**Returns the value produced by a run of this stream processor contained in the Monad M.
     * If M is lazily evaluated, this method is lazily evaluated.   If not, then this method could be dangerous.
     */
    def run(implicit m : Monad[M], s : StreamChunk[C,E]): M[A] = {
      def runCont(i: IterGV[C, E, M, A]) = i.fold(done = (x, _) => Some(x), cont = _ => None)
      fold[M[A]](
           done = (x, _) => m.pure(x),
           cont = { k =>
             k.apply(EOF[C,E]) map (i => runCont(i).getOrElse(error("Divergent iteratee")))
           }
          )
    }
  }
  /** A generic unfinished stream processor.  Iteratee's are Monadic and composable using for-expressions. */
  case class Iteratee[C[_], E, M[_], A]( iter : (=> Input[C, E]) => M[IterGV[C,E,M,A]]) {
    /** Defined so an Iteratee looks like a Input[C,E] => M[IterGV[C,E,M,A]] */
    def apply(i : => Input[C,E]) : M[IterGV[C,E,M,A]] = iter(i)

    def map[B](f : A => B)(implicit m : Monad[M]) : Iteratee[C,E,M,B] =
      Iteratee(input =>
        iter(input).flatMap { i =>
          i.fold(
            done = (a,chunk) => m.pure(Done(f(a), chunk)),
            cont = (i) =>  m.pure(Cont(i map f))
          )
        }
      )
    def flatMap[B](f : A => Iteratee[C,E,M,B])(implicit m : Monad[M]) : Iteratee[C,E,M,B] =
      Iteratee(input =>
        iter(input).flatMap { i =>
          i.fold(
           done = (a, chunk) => f(a)(chunk),
           cont = (i) => m.pure(Cont(i flatMap f))
          )
        }
      )

  }

  implicit def iterateePure[C[_], E, M[_]](implicit m : Monad[M]) = new Pure[({type I[A]=Iteratee[C,E,M,A]})#I] {
    def pure[A](a : => A) : Iteratee[C,E,M,A] = Iteratee[C,E,M,A](input => m.pure(Done(a, input)))
  }
  implicit def iterateeBind[C[_], E, M[_]](implicit m : Monad[M]) = new Bind[({type I[A]=Iteratee[C,E,M,A]})#I] {
    def bind[A,B](i : Iteratee[C,E,M, A], f : A => Iteratee[C,E,M,B]) = i flatMap f
  }
  implicit def iterateeMA[C[_], E, M[_], A](v: Iteratee[C, E, M, A]): MA[({type I[A] = Iteratee[C, E, M, A]})#I, A] =
    ma[({type I[A]=Iteratee[C, E, M, A]})#I, A](v)

  /** A computation that has finished **/
  object Done {
    def apply[C[_], E, M[_], A](a: => A, i: => Input[C, E]): IterGV[C, E, M, A] = new IterGV[C, E, M, A] {
      def fold[Z](done: (=> A, => Input[C, E]) => Z,
                  cont: (=> Iteratee[C, E, M, A]) => Z): Z = done(a, i)
    }
    def unapply[C[_], E, M[_], A](r: IterGV[C, E, M, A]): Option[(A, Input[C, E])] =
      r.fold[Option[(A,Input[C, E])]](
        done = (a, i) => Some((a, i)),
        cont = f => None)
  }

  /** A computation that takes an element from an input to yield a new computation **/
  object Cont {
    def apply[C[_],E, M[_],A](f : (=> Input[C,E]) => M[IterGV[C,E,M,A]]) :IterGV[C,E,M,A] = apply(Iteratee(f))
    def apply[C[_], E, M[_], A](f: Iteratee[C, E, M, A]): IterGV[C, E, M, A] = new IterGV[C, E, M, A] {
      def fold[Z](done: (=> A, => Input[C, E]) => Z,
                  cont: (=> Iteratee[C,E,M,A]) => Z): Z = cont(f)
    }
    def unapply[C[_], E, M[_], A](r: IterGV[C, E, M, A]): Option[Iteratee[C,E,M,A]] =
      r.fold[Option[Iteratee[C,E,M,A]]](
        done = (a, i) => None,
        cont = (i) => Some(i))
  }


  /** An Enumerator[F] feeds data from an F to an iteratee **/
  trait Enumerator[F[_]] {
    def apply[E, C[_], M[_], A](f: F[E], i: IterGV[C, E, M, A]): IterGV[C, E, M, A]
  }


  /** An iteratee that consumes the head of the input **/
  def head[C[_], E, M[_]](implicit m : Monad[M], s : StreamChunk[C,E]) : IterGV[C, E, M, Option[E]] = {
    def step(in: => Input[C, E]): M[IterGV[C, E, M, Option[E]]] =
      m.pure(in(chunk = e => if(s.length(e) > 0)
          Done(Some(s.head(e)), Chunk(s.tail(e)))
        else Cont(step _),
        failure = msg => Done(None, Failure[C,E](msg)),
        eof = Done(None, EOF[C,E])))
    Cont(step _)
  }

  /** An iteratee that returns the first element of the input **/
  def peek[C[_], E, M[_]](implicit s : StreamChunk[C,E], m : Monad[M]) : IterGV[C, E, M, Option[E]] = {
    def step(in: => Input[C, E]): M[IterGV[C, E, M, Option[E]]] =
      m.pure(in match {
        case Chunk(c) if s.length(c) == 0 => Cont(step _)          // TODO - Done(None, in)?
        case Chunk(c) => Done(Option(s.head(c)), in)
        case x => Done(None, x)
      })
    Cont(step _)
  }

    /** Peeks and returns either a Done iteratee with the given value or runs the given function with the peeked value **/
    //def peekDoneOr[A, B](b: => B, f: A => IterV[A, B]): IterV[A, B] =
    //  peek[A] >>= (_.iterDoneOr(b, f))

    /** An iteratee that skips the first n elements of the input **/
    //def drop[E](n: Int): IterV[E, Unit] = {
    //  def step(s: Input[E]): IterV[E, Unit] =
    //    s(el = _ => drop(n - 1),
    //      empty = Cont(step),
    //      eof = Done((), EOF[E]))
    //  if (n == 0) Done((), Empty[E])
    //  else Cont(step)
    //}

    /** An iteratee that counts and consumes the elements of the input **/
    //def length[E] : IterV[E, Int] = {
    //  def step(acc: Int)(s: Input[E]): IterV[E, Int] =
    //    s(el = _ => Cont(step(acc + 1)),
    //      empty = Cont(step(acc)),
    //      eof = Done(acc, EOF[E]))
    //  Cont(step(0))
    //}

    /**
     * Takes while the given predicate holds, appending with the given monoid.
     */
    //def takeWhile[A, F[_]](pred: A => Boolean)(implicit mon: Monoid[F[A]], pr: Pure[F]): IterV[A, F[A]] = {
    //  def peekStepDoneOr(z: F[A]) = peekDoneOr(z, step(z, _: A))
    //
    //  def step(acc: F[A], a: A): IterV[A, F[A]] = {
    //    if (pred(a))
    //      drop(1) >>=| peekStepDoneOr(acc |+| a.η[F])
    //    else
    //      Done(acc, EOF.apply)
    //  }
    //  peekStepDoneOr(∅[F[A]])
    //}

    /**
     * Produces chunked output split by the given predicate.
     */
    //def groupBy[A, F[_]](pred: (A, A) => Boolean)(implicit mon: Monoid[F[A]], pr: Pure[F]): IterV[A, F[A]] = {
    //  IterV.peek >>= {
    //    case None => Done(∅[F[A]], Empty[A])
    //    case Some(h) => takeWhile(pred(_, h))
    //  }
    //}

    /**
     * Repeats the given iteratee by appending with the given monoid.
     */
  //  def repeat[E,A, F[_]](iter: IterV[E,A])(implicit mon: Monoid[F[A]], pr: Pure[F]): IterV[E, F[A]] = {
  //	  def step(s: F[A]): Input[E] => IterV[E, F[A]] = {
  //	    case EOF() => Done(s, EOF.apply)
  //	    case Empty() => Cont(step(s))
  //	    case El(e) => iter match {
  //	      case Done(a, _) => Done(s |+| a.η[F], El(e))
  //	      case Cont(k) => for {
  //	        h <- k(El(e))
  //	        t <- repeat(iter)
  //	      } yield s |+| h.η[F] |+| t
  //	    }
  //	  }
  //	  Cont(step(∅[F[A]]))
  //	}


}