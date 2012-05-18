package scalaz
package parse
package state

import base._
import Result._
import std.list._
import std.string._


abstract class Parser[S, T, A](implicit FR: Functor[({type l[a] = Result[(List[T], S), a]})#l]) {
  def apply(s: S, ts: List[T]): Result[(List[T], S), A]

  def map[B](f: A => B): Parser[S, T, B] = Parser((s, ts) => FR.map(apply(s, ts))(f))

  def flatMap[B](f: A => Parser[S, T, B]): Parser[S, T, B] = {
    def continue(res: Result[(List[T], S), A]): Result[(List[T], S), B] = res match {
      case ResultSuccess((ts, s), x) => f(x)(s, ts)
      case ResultCommitted(c@ResultCommitted(r)) => continue(c)
      case ResultCommitted(r) => ResultCommitted(continue(r))
      case ResultFailure(tss, e) => ResultFailure(tss, e)
    }
    Parser((s, ts) => continue(apply(s, ts)))
  }
}


trait StateParserInstances {
  this: StateParserFunctions =>

  implicit def parserMonadFail[S, T](implicit FR: Functor[({type l[a] = Result[(List[T], S), a]})#l], C: Commitment[({type λ[α]=Parser[S, T, α]})#λ]): MonadPlus[({type l[a]=Parser[S, T, a]})#l] = new MonadPlus[({type l[a]=Parser[S, T, a]})#l] {
    def point[A](a: => A) = Parser((s, ts) => ResultSuccess((ts, s), a))

    override def map[A, B](fa: Parser[S, T, A])(f: (A) => B) = fa.map(f)

    override def ap[A, B](fa: => Parser[S, T, A])(ff: => Parser[S, T, (A) => B]) = for {
      f <- ff
      a <- fa
    } yield f(a)

    def bind[A, B](fa: Parser[S, T, A])(f: (A) => Parser[S, T, B]) = fa.flatMap(f)

    def empty[A] = C.fail(NoParse)

    def plus[A](a: Parser[S, T, A], b: => Parser[S, T, A]) = onFail(a, b)
  }


  implicit def stateParserCommitment[S, T]: Commitment[({type l[a]=Parser[S, T, a]})#l] = new Commitment[({type l[a]=Parser[S, T, a]})#l] {
    def commit[A](p: Parser[S, T, A]) = Parser((s, ts) => ResultCommitted(p(s, ts)))

    def fail[A](reason: FailureReason) = Parser((s, ts) => ResultFailure((ts, s), reason))

    def oneOf[A](t: List[Parser[S, T, A]]) = {
      def accum(errs: List[FailureReason], ts: List[Parser[S, T, A]]): Parser[S, T, A] = ts match {
        case Nil => fail(CouldNotParseAnyOf(errs))

        case parse :: ps => Parser[S, T, A]((s, tts) => (parse(s, tts) match {
          case ResultFailure(_, err) => accum(err :: errs, ps)(s, tts)
          case r @ ResultSuccess(_, _) => r
          case r @ ResultCommitted(_) => r
        }))
      }

      accum(List.empty, t)

    }
  }

  implicit def parserPolyParse[S, T](implicit
    FP: Alternative[({type λ[α]=Parser[S, T, α]})#λ],
    C: Commitment[({type λ[α]=Parser[S, T, α]})#λ],
    M: MonadPlus[({type λ[α]=Parser[S, T, α]})#λ]) : PolyParse[({type l[a]=Parser[S, T, a]})#l] = PolyParse[({type l[a]=Parser[S, T, a]})#l]
}


trait StateParserFunctions {
  def onFail[S, T, A](fa : => Parser[S, T, A], fb: => Parser[S, T, A]): Parser[S, T, A] = {
    def continue(s: S, ts: List[T], res: Result[(List[T], S), A]): Result[(List[T], S), A] = res match {
      case ResultFailure(_, _) => fb(s, ts)
      case r => r
    }
    Parser((s, ts) => continue(s, ts, fa(s, ts)))
  }

  def next[S, T]: Parser[S, T, T] = Parser((s, ts) => ts match {
    case Nil => ResultFailure((Nil, s), UnexpectedEOF)
    case t :: tts => ResultSuccess((tts, s), t)
  })

  def eof[S, T]: Parser[S, T, Unit] = Parser((s, ts) => ts match {
    case Nil => ResultSuccess((Nil, s), ())
    case t :: tts => ResultFailure((ts, s), UnexpectedInput(t))
  })

  def satisfy[S, T](pred: T => Boolean)(implicit M: Monad[({type l[a]=Parser[S, T, a]})#l], C: Commitment[({type l[a]=Parser[S, T, a]})#l]): Parser[S, T, T] = {
    for {
      x <- next
      y <- if(pred(x)) M.pure(x) else C.fail(Failed)
    } yield y
  }


  def stUpdate[S, T](f: S => S): Parser[S, T, Unit] = Parser((s, ts) => ResultSuccess((ts, f(s)), ()))

  def stQuery[S, T, A](f: S => A): Parser[S, T, A] = Parser((s, ts) => ResultSuccess((ts, s), f(s)))

  def stGet[S, T]: Parser[S, T, S] = Parser((s, ts) => ResultSuccess((ts, s), s))

  def reparse[S, T](ts: List[T]): Parser[S, T, Unit] = Parser((s, input) => ResultSuccess((ts ++ input, s), ()))


}



object Parser extends StateParserFunctions with StateParserInstances {
  def apply[S, T, A](f: (S, List[T]) => Result[(List[T], S), A]): Parser[S, T, A] = new Parser[S, T, A] {
    def apply(s: S, ts: List[T]) = f(s, ts)
  }

  def runParser[S, T, A](parser: Parser[S, T, A], state: S, tokens: List[T]): (Either[FailureReason, A], S, List[T]) = {
    val (either, (ts, s)) = resultToEither(parser(state, tokens))
    (either, s, ts)
  }

//  def unapply[S, T, A](p: Parser[S, T, A]): Option[(S, List[T]) => Result[(List[T], S), A]] = Some(p.apply(_, _))
}