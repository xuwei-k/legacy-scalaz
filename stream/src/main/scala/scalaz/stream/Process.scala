package scalaz.stream

import scala.collection.immutable.IndexedSeq

import scalaz.{Catchable,Monad,Leibniz}
import scalaz.concurrent.Task
import Leibniz.{===, witness}

/** 
 * A `Process[F,O]` represents a stream of `O` values which can interleave 
 * external requests to evaluate expressions of the form `F[A]`. It takes
 * the form of a state machine with three possible states: `Emit`, which 
 * indicates that `h` should be emitted to the output stream, `Halt`,
 * which indicates that the `Process` is finished making requests and 
 * emitting values to the output stream, and `Await` which asks the driver 
 * to evaluate some `F[A]` and resume processing once the result is available. 
 * See the constructor definitions in the `Process` companion object.
 */
trait Process[F[_],+O] {
  
  import Process._

  /** Transforms the output values of this `Process` using `f`. */
  final def map[O2](f: O => O2): Process[F,O2] = Process.map(this)(f)

  /** 
   * Generate a `Process` dynamically for each output of this `Process`, and
   * sequence these processes using `++`. 
   */
  final def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] = 
    Process.bind(this)(f)
  
  /** Run this `Process`, then, if it halts without an error, run `p2`. */
  final def append[O2>:O](p2: => Process[F,O2]): Process[F,O2] = 
    Process.append(this: Process[F,O2])(p2)

  /** Operator alias for `append`. */
  final def ++[O2>:O](p2: => Process[F,O2]): Process[F,O2] = 
    Process.append(this: Process[F,O2])(p2)

  /** 
   * Run this process until it halts, then run it again and again, as
   * long as no errors occurt. 
   */
  final def repeat: Process[F,O] = 
    Process.repeat(this)

  /** 
   * Collect the outputs of this `Process[F,O]`, given a `Monad[F]` in
   * which we can catch exceptions. This function is not tail recursive and
   * relies on the `Monad[F]` to ensure stack safety. 
   */
  final def collect[O2>:O](implicit F: Monad[F], C: Catchable[F]): F[IndexedSeq[O2]] = 
    Process.collect(this: Process[F,O2])(F,C)

  /**
   * Ignores output of this `Process`. A drained `Process` will never `Emit`.  
   */
  final def drain[O2]: Process[F,O2] = 
    Process.drain(this)

  /**
   * Halt this process, but give it an opportunity to run any requests it has 
   * in the `cleanup` argument of its next `Await`.
   */
  final def kill[O2]: Process[F,O2] =
    Process.kill(this)

  /** 
   * Feed the output of this `Process` as input of `p2`. The implementation  
   * will fuse the two processes, so this process will only generate
   * values as they are demanded by `p2`. If `p2` signals termination, `this`
   * is killed using `kill`, giving it the opportunity to clean up. 
   */
  final def pipe[O2 >: O, O3](p2: Process1[O2,O3]): Process[F,O3] = 
    Process.pipe(this: Process[F,O2])(p2)

  /** Operator alias for `pipe`. */
  final def |>[O2 >: O, O3](p2: Process1[O2,O3]): Process[F,O3] = 
    Process.pipe(this: Process[F,O2])(p2)

  /* 
   * Use a `Tee` to interleave or combine the outputs of `this` and
   * `p2`. This can be used for zipping, interleaving, and so forth.
   * Nothing requires that the `Tee` read elements from each 
   * `Process` in lockstep. It could read fifty elements from one 
   * side, then two elements from the other, then combine or
   * interleave these values in some way, etc.
   * 
   * The definition uses two helper functions, `feedL` and `feedR`,
   * which feed the `Tee` in a tail-recursive loop as long as
   * it is awaiting input from either side.
   */ 
  final def tee[O2 >: O,O3,O4](p2: Process[F,O3])(t: Tee[O2,O3,O4]): Process[F,O4] = 
    Process.tee(this: Process[F,O2], p2)(t)

  // iamhere - just adding rest of the process functions
}

object Process {
  case class Await[F[_],A,+O](
    req: F[A], recv: A => Process[F,O],
    fallback: Process[F,O],
    cleanup: Process[F,O]) extends Process[F,O]

  case class Emit[F[_],O](
    head: Seq[O], 
    tail: Process[F,O]) extends Process[F,O]

  case class Halt[F[_],O]() extends Process[F,O]

  def map[F[_],O,O2](p: Process[F,O])(f: O => O2): Process[F,O2] = p match {
    case Await(req,recv,fb,c) => 
      Await[F,Any,O2](req, recv andThen (map(_)(f)), map(fb)(f), map(c)(f)) 
    case Emit(h, t) => Emit[F,O2](h map f, map(t)(f))
    case Halt() => Halt()
  }

  def append[F[_],O](p: Process[F,O])(p2: => Process[F,O]): Process[F,O] = p match {
    case Halt() => p2
    case Emit(h, t) => emitAll(h, append(t)(p2))
    case Await(req,recv,fb,c) => 
      Await(req, recv andThen (append(_)(p2)), append(fb)(p2), c)
  }

  def bind[F[_],O,O2](p: Process[F,O])(f: O => Process[F,O2]): Process[F,O2] =
    p match {
      case Halt() => Halt()
      case Emit(o, t) => 
        if (o.isEmpty) t.flatMap(f)
        else f(o.head) ++ emitAll(o.tail, t).flatMap(f)
      case Await(req,recv,fb,c) => 
        Await(req, recv andThen (_ flatMap f), fb flatMap f, c flatMap f)
    }

  def repeat[F[_],O](p: Process[F,O]): Process[F,O] = {
    def go(cur: Process[F,O]): Process[F,O] = cur match {
      case Halt() => go(p)
      case Await(req,recv,fb,c) => Await(req, recv andThen go, fb, c)
      case Emit(h, t) => emitAll(h, go(t))
    }
    go(p)
  }

  @annotation.tailrec
  def kill[F[_],O,O2](p: Process[F,O]): Process[F,O2] = p match {
    case Await(req,recv,fb,c) => c.drain 
    case Halt() => Halt()
    case Emit(h, t) => kill[F,O,O2](t)
  }

  def drain[F[_],O,O2](p: Process[F,O]): Process[F,O2] = p match {
    case Halt() => Halt()
    case Emit(h, t) => drain(t)
    case Await(req,recv,fb,c) => Await(
      req, recv andThen drain, 
      drain(fb), drain(c)) 
  }

  /* 
   * We define `Process1` as a type alias - see the companion object
   * for `Process` below. Using that, we can then define `|>` once
   * more. The definition is extremely similar to our previous 
   * definition. 
   * 
   * The one subtlety is we make sure that if `p2` halts, we
   * `kill` this process, giving it a chance to run any cleanup
   * actions (like closing file handles, etc). 
   */
  def pipe[F[_],O,O2](p: Process[F,O])(p2: Process1[O,O2]): Process[F,O2] = {
    // if this is emitting values and p2 is consuming values,
    // we feed p2 in a loop to avoid using stack space
    @annotation.tailrec
    def feed(emit: Seq[O], tail: Process[F,O], 
             recv: O => Process1[O,O2], 
             fb: Process1[O,O2],
             cleanup: Process1[O,O2]): Process[F,O2] = 
      if (emit isEmpty) tail |> await1(recv, fb) 
      else recv(emit.head) match {
        case Await(_, recv2, fb2, c2) => 
          feed(emit.tail, tail, recv2, fb2, c2)
        case p => pipe(emitAll(emit.tail, tail))(p)
      }
    p2 match {
      case Halt() => p.kill ++ Halt()
      case Emit(h, t) => emitAll(h, pipe(p)(t))
      case Await(req,recv,fb,c) => p match {
        case Emit(h,t) => feed(h, t, recv, fb, c)
        case Halt() => pipe[F,O,O2](Halt())(fb)
        case Await(req0,recv0,fb0,c0) => 
          await(req0)(i => pipe(recv0(i))(p2), 
                      pipe(fb0)(fb),
                      pipe(c0)(c))
      }
    }
  }

  def tee[F[_],O,O2,O3](p: Process[F,O], p2: Process[F,O2])(t: Tee[O,O2,O3]): Process[F,O3] = {
    @annotation.tailrec
    def feedL(emit: Seq[O], tail: Process[F,O], 
              other: Process[F,O2],
              recv: O => Tee[O,O2,O3], 
              fb: Tee[O,O2,O3],
              c: Tee[O,O2,O3]): Process[F,O3] = 
      if (emit isEmpty) (tail tee other)(await(L[O,O2])(recv, fb, c))
      else recv(emit.head) match {
        case t2@Await(e, recv2, fb2, c2) => e.get match {
          case Left(_) => feedL(emit.tail, tail, other, recv2, fb2, c2)
          case _ => (Emit(emit.tail, tail) tee other)(t2)
        }
        case p => (Emit(emit.tail, tail) tee other)(p)
      }
    @annotation.tailrec
    def feedR(emit: Seq[O2], tail: Process[F,O2], 
              other: Process[F,O],
              recv: O2 => Tee[O,O2,O3], 
              fb: Tee[O,O2,O3],
              c: Tee[O,O2,O3]): Process[F,O3] = 
      if (emit isEmpty) (other tee tail)(await(R[O,O2])(recv, fb, c))
      else recv(emit.head) match {
        case t2@Await(e, recv2, fb2, c2) => e.get match {
          case Right(_) => feedR(emit.tail, tail, other, recv2, fb2, c2)
          case _ => (other tee Emit(emit.tail, tail))(t2)
        }
        case p => (other tee Emit(emit.tail, tail))(p)
      }
    t match {
      case Halt() => p.kill ++ p2.kill ++ Halt() 
      case Emit(h,t) => Emit(h, (p tee p2)(t))
      case Await(side, recv, fb, c) => side.get match {
        case Left(isO) => p match {
          case Halt() => p2.kill ++ Halt()
          case Emit(o,ot) => feedL(o, ot, p2, witness(isO) andThen recv, fb, c) 
          case Await(reqL, recvL, fbL, cL) => 
            Await(reqL, recvL andThen (pnext => (pnext tee p2)(t)), 
                  (fbL tee p2)(t), (cL tee p2)(t))
        }
        case Right(isO2) => p2 match {
          case Halt() => p.kill ++ Halt()
          case Emit(o,ot) => feedR(o, ot, p, witness(isO2) andThen recv, fb, c)
          case Await(reqR, recvR, fbR, cR) => 
            Await(reqR, recvR andThen (p3 => (p tee p3)(t)), 
                  (p tee fbR)(t), (p tee cR)(t))
        }
      }
    }
  }

  def collect[F[_],O](p: Process[F,O])(
                      implicit F: Monad[F], C: Catchable[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F,O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = 
      cur match {
        case Emit(h,t) => go(t, acc ++ h) 
        case Halt() => F.point(acc)
        case Await(req,recv,fb,c) => 
           F.bind (C.attempt(req)) {
             case Left(End) => go(fb, acc)
             case Left(err) => 
               go(c ++ await[F,Nothing,O](C.fail(err))(), acc)
             case Right(o) => go(recv(o), acc)
           }
      }
    go(p, IndexedSeq())
  }
    

  def emitAll[F[_],O](
      head: Seq[O], 
      tail: Process[F,O] = Halt[F,O]()): Process[F,O] = 
    tail match {
      case Emit(h2,t) => Emit(head ++ h2, t)
      case _ => Emit(head, tail)
    }
  def emit[F[_],O](
      head: O, 
      tail: Process[F,O] = Halt[F,O]()): Process[F,O] = 
    emitAll(Stream(head), tail)

  def await[F[_],A,O](req: F[A])(
      recv: A => Process[F,O] = (a: A) => Halt[F,O](), 
      fallback: Process[F,O] = Halt[F,O](),
      cleanup: Process[F,O] = Halt[F,O]()): Process[F,O] = 
    Await(req, recv, fallback, cleanup)

  /* Special exception indicating normal termination */
  case object End extends Exception {
    override def fillInStackTrace = this 
  }
 
  /** 
   * A simple tail recursive function to collect all the output of a 
   * `Process[Task,O]`. Because `Task` has a `run` function,
   * we can implement this as a tail-recursive function. 
   */
  def collectTask[O](src: Process[Task,O]): IndexedSeq[O] = {
    @annotation.tailrec
    def go(cur: Process[Task,O], acc: IndexedSeq[O]): IndexedSeq[O] = 
      cur match {
        case Emit(h,t) => go(t, acc ++ h) 
        case Halt() => acc
        case Await(req,recv,fb,err) =>
          val next = 
            try recv(req.run)
            catch { 
              case End => fb // Normal termination
              case e: Exception => err ++ failTask(e) // Helper function, defined below
            }
          go(next, acc)
      }
    go(src, IndexedSeq()) 
  }

  def failTask[O](e: Throwable): Process[Task,O] = 
    await[Task,O,O](Task(throw e))()

  /* 
   * We can write a version of collect that works for any `Monad`. 
   * See the definition in the body of `Process`. 
   */

  /* 
   * Generic combinator for producing a `Process[Task,O]` from some
   * effectful `O` source. The source is tied to some resource,
   * `R` (like a file handle) that we want to ensure is released.
   * See `lines` below for an example use. 
   */
  def resource[R,O](acquire: Task[R])(
                    release: R => Task[Unit])(
                    step: R => Task[O]): Process[Task,O] = {
    def go(step: Task[O], onExit: Task[Unit]): Process[Task,O] =
      await[Task,O,O](step) ( 
        o => emit(o, go(step, onExit)) // Emit the value and repeat 
      , await[Task,Unit,O](onExit)()  // Release resource when exhausted
      , await[Task,Unit,O](onExit)()) // or in event of error
    await(acquire) ( r => go(step(r), release(r)), Halt(), Halt() )
  }
  
  /* 
   * We now have nice, resource safe effectful sources, but we don't 
   * have any way to transform them or filter them. Luckily we can 
   * still represent the single-input `Process` type we introduced
   * earlier, which we'll now call `Process1`.  
   */

  case class Is[I]() {
    sealed trait f[X] { def is: X === I } // see definition in `Eq.scala`
    val Get = new f[I] { def is = Leibniz.refl }
  }
  def Get[I] = Is[I]().Get

  type Process1[I,O] = Process[Is[I]#f, O]
  
  def halt1[I,O]: Process1[I,O] = Halt[Is[I]#f, O]()

  def await1[I,O](
    recv: I => Process1[I,O],
    fallback: Process1[I,O] = halt1[I,O]): Process1[I, O]  = 
    Await(Get[I], recv, fallback, halt1)

  def emit1[I,O](h: O, tl: Process1[I,O] = halt1[I,O]): Process1[I,O] = 
    emit(h, tl)
  
  def emitAll1[I,O](h: Seq[O], tl: Process1[I,O] = halt1[I,O]): Process1[I,O] = 
    emitAll(h, tl)

  def lift[I,O](f: I => O): Process1[I,O] = 
    await1[I,O]((i:I) => emit(f(i))) repeat
  
  def filter[I](f: I => Boolean): Process1[I,I] =
    await1[I,I](i => if (f(i)) emit(i) else halt1) repeat

  // we can define take, takeWhile, and so on as before

  def take[I](n: Int): Process1[I,I] = 
    if (n <= 0) halt1
    else await1[I,I](i => emit(i, take(n-1)))

                          /*                       

  We sometimes need to construct a `Process` that will pull values
  from multiple input sources. For instance, suppose we want to 
  'zip' together two files, `f1.txt` and `f2.txt`, combining
  corresponding lines in some way. Using the same trick we used for
  `Process1`, we can create a two-input `Process` which can request
  values from either the 'left' stream or the 'right' stream. We'll
  call this a `Tee`, after the letter 'T', which looks like a 
  little diagram of two inputs being combined into one output. 

                           */

  case class T[I,I2]() {
    sealed trait f[X] { def get: Either[X === I, X === I2] }
    val L = new f[I] { def get = Left(Leibniz.refl) }
    val R = new f[I2] { def get = Right(Leibniz.refl) }
  }
  def L[I,I2] = T[I,I2]().L
  def R[I,I2] = T[I,I2]().R

  type Tee[I,I2,O] = Process[T[I,I2]#f, O]
  
  /* Again some helper functions to improve type inference. */

  def awaitL[I,I2,O](
      recv: I => Tee[I,I2,O], 
      fallback: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
    await[T[I,I2]#f,I,O](L)(recv, fallback)

  def awaitR[I,I2,O](
      recv: I2 => Tee[I,I2,O], 
      fallback: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
    await[T[I,I2]#f,I2,O](R)(recv, fallback)

  def haltT[I,I2,O]: Tee[I,I2,O] = 
    Halt[T[I,I2]#f,O]()

  def emitT[I,I2,O](h: O, tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
    emit(h, tl)
  
  def emitAllT[I,I2,O](h: Seq[O], tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
    emitAll(h, tl)

  def zipWith[I,I2,O](f: (I,I2) => O): Tee[I,I2,O] = 
    awaitL[I,I2,O](i  => 
    awaitR        (i2 => emitT(f(i,i2)))) repeat

  def zip[I,I2]: Tee[I,I2,(I,I2)] = zipWith((_,_))

  /* 
   * Like `zip` on lists, the above version halts as soon as either
   * input is exhausted. Here is a version that pads the shorter
   * stream with values. 
   */
   
  def zipWithAll[I,I2,O](padI: I, padI2: I2)(
                         f: (I,I2) => O): Tee[I,I2,O] = {
    val fbR = passR[I,I2] map (f(padI, _    ))                     
    val fbL = passL[I,I2] map (f(_   , padI2))
    awaitLOr(fbR)(i => 
    awaitROr(fbL)(i2 => emitT(f(i,i2)))) repeat
  }

  def zipAll[I,I2](padI: I, padI2: I2): Tee[I,I2,(I,I2)] = 
    zipWithAll(padI, padI2)((_,_))
  
  def awaitLOr[I,I2,O](fallback: Tee[I,I2,O])(
                       recvL: I => Tee[I,I2,O]): Tee[I,I2,O] =
    awaitL(recvL, fallback)

  def awaitROr[I,I2,O](fallback: Tee[I,I2,O])(
                       recvR: I2 => Tee[I,I2,O]): Tee[I,I2,O] =
    awaitR(recvR, fallback)

  /* Ignores all input from left. */
  def passR[I,I2]: Tee[I,I2,I2] = awaitR(emitT(_, passR))
  
  /* Ignores input from the right. */
  def passL[I,I2]: Tee[I,I2,I] = awaitL(emitT(_, passL))
  
  /* Alternate pulling values from the left and the right inputs. */
  def interleaveT[I]: Tee[I,I,I] = 
    awaitL[I,I,I](i =>
    awaitR       (i2 => emitAllT(Seq(i,i2)))) repeat

                          /*                       

  Our `Process` type can also represent effectful sinks (like a file).
  A `Sink` is simply a source of effectful functions! See the
  definition of `to` in `Process` for an example of how to feed a 
  `Process` to a `Sink`.

                           */

  type Sink[F[_],O] = Process[F, O => F[Unit]]

  def eval[F[_],O](p: Process[F, F[O]]): Process[F,O] = 
    p match {
      case Halt() => Halt()
      case Emit(h, t) => 
        if (h.isEmpty) eval(t)
        else await[F,O,O](h.head)(o => emit(o, eval(emitAll(h.tail, t))))
      case Await(req,recv,fb,c) => 
        await(req)(recv andThen eval, eval(fb), eval(c)) 
    }
  
  /* Infix syntax for `eval`. */
  class EvalProcess[F[_],O](self: Process[F,F[O]]) {
    def eval = Process.eval(self) 
  }
  implicit def EvalProcess[F[_],O](p: Process[F,F[O]]): EvalProcess[F,O] = 
    new EvalProcess(p)

  type Channel[F[_],I,O] = Process[F, I => F[O]]
}
