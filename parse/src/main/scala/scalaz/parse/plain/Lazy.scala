package scalaz
package parse
package plain

import base._


case class LazyParser[T, A](parser: Parser[T, A])(implicit R: Functor[({type λ[α]=Result[List[T], α]})#λ]) {
  def apply(ts: List[T]): Result[List[T], A] = parser.apply(ts)

  def flatMap[B](f: A => LazyParser[T, B]): LazyParser[T, B] = {
    def continue(r: Result[List[T], A]): Result[List[T], B] = r match {
      case ResultSuccess(ts, a) => f(a).parser.apply(ts)
      case ResultCommitted(ResultCommitted(r)) => continue(ResultCommitted(r))
      case ResultCommitted(r) => ResultCommitted(continue(r))
      case ResultFailure(ts, e) => ResultFailure(ts, e)
    }
    LazyParser(Parser(ts => continue(this.parser.apply(ts))))
  }

  def map[B](f: A => B): LazyParser[T, B] = LazyParser(parser.map(f))
}

object LazyParser extends LazyPlainParserInstances with LazyPlainParserFunctions {

  def runParser[T, A](pa: LazyParser[T, A], ts: List[T]): (Either[FailureReason, A], List[T]) = {
    Result.resultToEither(pa(ts))
  }
}

trait LazyPlainParserFunctions {
  import plain.{Parser => P}

  def onFail[T, A](fa: => LazyParser[T, A], fb: => LazyParser[T, A]): LazyParser[T, A] = LazyParser(P.onFail(fa.parser, fb.parser))
  def next[T]: LazyParser[T, T] = LazyParser(P.next[T])
  def eof[T]: LazyParser[T, Unit] = LazyParser(P.eof[T])
  def satisfy[T](pred: T => Boolean)(implicit M: Monad[({type λ[α] = Parser[T, α]})#λ], C: Commitment[({type λ[α] = Parser[T, α]})#λ]): LazyParser[T, T] = LazyParser(P.satisfy(pred))
  def reparse[T](ts: List[T]): LazyParser[T, Unit] = LazyParser(P.reparse(ts))
}

trait LazyPlainParserInstances {
  import LazyParser._

  implicit def lazyParserCommitment[T](implicit C: Commitment[({type λ[α] = Parser[T, α]})#λ], M: Monad[({type λ[α] = LazyParser[T, α]})#λ]): Commitment[({type λ[α] = LazyParser[T, α]})#λ] = new Commitment[({type λ[α] = LazyParser[T, α]})#λ] {
    def commit[A](fa: LazyParser[T, A]): LazyParser[T, A] = LazyParser(C.commit(fa.parser))


    def fail[A](reason: FailureReason) = LazyParser(C.fail(reason))

    def oneOf[A](t: List[LazyParser[T, A]]): LazyParser[T, A] = LazyParser(C.oneOf(t.map(_.parser)))
  }


  implicit def lazyParserMonadPlus[T](implicit M: MonadPlus[({type λ[α] = Parser[T, α]})#λ], C: Commitment[({type λ[α] = Parser[T, α]})#λ], RF: Functor[({type λ[α]=Result[List[T], α]})#λ]): Monad[({type λ[α]=LazyParser[T, α]})#λ] = new MonadPlus[({type λ[α]=LazyParser[T, α]})#λ] {
    def bind[A, B](fa: LazyParser[T, A])(f: (A) => LazyParser[T, B]) = LazyParser(M.bind(fa.parser)(a => f(a).parser))

    def point[A](a: => A): LazyParser[T, A] = LazyParser(M.point(a))

    override def ap[A, B](fa: => LazyParser[T, A])(ff: => LazyParser[T, (A) => B]): LazyParser[T, B] = {
      def continue(res: Result[List[T], A => B]): Result[List[T], B] = res match {
        case ResultSuccess(z, f) => runParser(fa, z) match {
          case (Left(e), ts) => ResultFailure(ts, e)
          case (Right(x), ts) => ResultSuccess(ts, f(x))
        }
        case ResultCommitted(r) => ResultCommitted(continue(r))
        case ResultFailure(z, e) => ResultFailure(z, e)
      }
      LazyParser(Parser(ts => continue(ff(ts))))
    }

    def plus[A](a: LazyParser[T, A], b: => LazyParser[T, A]): LazyParser[T, A] = LazyParser(M.plus(a.parser, b.parser))

    def empty[A]: LazyParser[T, A] = LazyParser(M.empty)
  }

  implicit def parserPolyParse[T](implicit
    C: Commitment[({type λ[α]=LazyParser[T, α]})#λ],
    M: MonadPlus[({type λ[α]=LazyParser[T, α]})#λ]) : PolyParse[({type λ[α]=LazyParser[T, α]})#λ] = PolyParse[({type λ[α]=LazyParser[T, α]})#λ]

}
