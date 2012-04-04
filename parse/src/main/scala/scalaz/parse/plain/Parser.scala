package scalaz
package parse
package plain

import base._

abstract class Parser[T, A](implicit R: Functor[({type λ[α]=Result[List[T], α]})#λ]) {
  def apply(l: List[T]): Result[List[T], A]

  def flatMap[B](f: A => Parser[T, B]): Parser[T, B] = {
    def continue(r: Result[List[T], A]): Result[List[T], B] = r match {
      case ResultSuccess(ts, a) => f(a).apply(ts)
      case ResultCommitted(ResultCommitted(r)) => continue(ResultCommitted(r))
      case ResultCommitted(r) => ResultCommitted(continue(r))
      case ResultFailure(ts, e) => ResultFailure(ts, e)
    }
    Parser(ts => continue(this.apply(ts)))
  }

  def map[B](f: A => B): Parser[T, B] = Parser(ts => R.map(this.apply(ts))(f))

}

object Parser extends ParserFunctions with ParserInstances {
  def apply[T, A](f: => List[T] => Result[List[T], A]): Parser[T, A] = new Parser[T, A] {
    def apply(ts: List[T]) = f(ts)
  }

  def runParser[T, A](parser: Parser[T, A], ts: List[T]): (Either[FailureReason, A], List[T]) = Result.resultToEither(parser.apply(ts))
}

trait ParserFunctions {
  def onFail[T, A](fa: => Parser[T, A], fb: => Parser[T, A]): Parser[T, A] = {
    def continue(ts: List[T], r: Result[List[T], A]) = r match {
      case ResultFailure(z, e) => fb.apply(ts)
      case r => r
    }
    Parser(ts => continue(ts, fa.apply(ts)))
  }

  def next[T]: Parser[T, T] = {
    Parser(ts => ts match {
      case Nil => ResultFailure(Nil, UnexpectedEOF)
      case (t :: tts) => ResultSuccess(tts, t)
    })
  }

  def eof[T]: Parser[T, Unit] = {
    Parser(ts => ts match {
      case Nil => ResultSuccess(Nil, ())
      case (t :: tts) => ResultFailure(tts, UnexpectedInput(t))
    })
  }

  def satisfy[T](pred: T => Boolean)(implicit M: Monad[({type λ[α] = Parser[T, α]})#λ], C: Commitment[({type λ[α] = Parser[T, α]})#λ]): Parser[T, T] = {
    for {
      x <- next[T]
      y <- if (pred(x)) M.pure(x) else C.fail[T](Failed)
    } yield y
  }

  def reparse[T](ts: List[T]): Parser[T, Unit] = {
    Parser(inp => ResultSuccess(ts ++ inp, ()))
  }
}

trait ParserInstances0 {
  this: ParserFunctions =>

  implicit def parserCommitment[T]: Commitment[({type λ[α] = Parser[T, α]})#λ] = new Commitment[({type λ[α] = Parser[T, α]})#λ] {
    def commit[A](p: Parser[T, A]) = Parser(ts => ResultCommitted(p.apply(ts)))

    def fail[A](reason: FailureReason) =  Parser(ts => ResultFailure(ts, reason))

    def oneOf[A](t: List[Parser[T, A]]): Parser[T, A] = {
      def accum(errs: List[FailureReason], ts: List[Parser[T, A]]): Parser[T, A] = ts match {
        case Nil => fail(CouldNotParseAnyOf(errs))

        case parse :: ps => Parser(tts => parse(tts) match {
          case ResultFailure(_, err) => accum(err :: errs, ps).apply(tts)
          case r @ ResultSuccess(_, _) => r
          case r @ ResultCommitted(_) => r
        })
      }

      accum(List.empty, t)
    }
  }

  implicit def parserMonadPlus[T](implicit RF: Functor[({type λ[α]=Result[List[T], α]})#λ], C: Commitment[({type λ[α]=Parser[T, α]})#λ]): MonadPlus[({type λ[α]=Parser[T, α]})#λ] = new MonadPlus[({type λ[α]=Parser[T, α]})#λ]{
    def point[A](a: => A) = Parser(ts => ResultSuccess(ts, a))

    override def map[A, B](fa: Parser[T, A])(f: (A) => B): Parser[T, B] = fa.map(f)

    override def ap[A, B](fa: => Parser[T, A])(ff: => Parser[T, A=>B]) = {
      for {
        f <- ff
        a <- fa
      } yield f(a)
    }

    def empty[A] = C.fail(NoParse)

    def plus[A](a: Parser[T, A], b: => Parser[T, A]): Parser[T, A] = Parser.onFail[T, A](a, b)

    def bind[A, B](fa: Parser[T, A])(f: (A) => Parser[T, B]): Parser[T, B] = fa.flatMap(f)

  }
}

trait ParserInstances extends ParserInstances0 {
  this: ParserFunctions =>

  implicit def parserPolyParse[T](implicit
    C: Commitment[({type λ[α]=Parser[T, α]})#λ],
    M: MonadPlus[({type λ[α]=Parser[T, α]})#λ]) : PolyParse[({type λ[α]=Parser[T, α]})#λ] = PolyParse[({type λ[α]=Parser[T, α]})#λ]
}
