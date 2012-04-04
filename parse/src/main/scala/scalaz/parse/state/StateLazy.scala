package scalaz
package parse
package state

import base._
import Result._

case class LazyParser[S, T, A](parser: Parser[S, T, A])(implicit FR: Functor[({type λ[α]=Result[(List[T], S), α]})#λ]) {
  def apply(s: S, ts: List[T]): Result[(List[T], S), A] = parser(s, ts)

  def map[B](f: A => B): LazyParser[S, T, B] = LazyParser(parser.map(f))

  def flatMap[B](f: A => LazyParser[S, T, B]): LazyParser[S, T, B] = {
    def continue(res: Result[(List[T], S), A]): Result[(List[T], S), B] = res match {
      case ResultSuccess((ts, s), x) => f(x).parser(s, ts)
      case ResultCommitted(c@ResultCommitted(r)) => continue(c)
      case ResultCommitted(r) => ResultCommitted(continue(r))
      case ResultFailure(tss, e) => ResultFailure(tss, e)
    }
    LazyParser(Parser((s, ts) => continue(parser(s, ts))))
  }
}

trait LazyParserInstances {
  import LazyParser._

  implicit def lazyParserMonadPlus[S, T](implicit M: MonadPlus[({type l[a]=Parser[S, T, a]})#l]): MonadPlus[({type l[a]=LazyParser[S, T, a]})#l] = new MonadPlus[({type l[a]=LazyParser[S, T, a]})#l] {

    def bind[A, B](fa: LazyParser[S, T, A])(f: (A) => LazyParser[S, T, B]): LazyParser[S, T, B] = fa.flatMap(f)

    override def map[A, B](fa: LazyParser[S, T, A])(f: (A) => B) = fa.map(f)

    def point[A](a: => A): LazyParser[S, T, A] = LazyParser(M.point(a))

    override def ap[A, B](fa: => LazyParser[S, T, A])(ff: => LazyParser[S, T, (A) => B]) = {
      def continue(res: Result[(List[T], S), A=>B]): Result[(List[T], S), B] = res match {
        case ResultSuccess((z, s), f) =>
          val (either, ss, zz) = runParser(fa, s, z)
          either match {
            case Left(reason) => ResultFailure((zz, ss), reason)
            case Right(x) => ResultSuccess((zz, ss), f(x))
          }
        case ResultFailure(zs, e) => ResultFailure(zs, e)
        case ResultCommitted(r) => ResultCommitted(continue(r))
      }

      LazyParser(Parser((s, ts) => continue(ff(s, ts))))
    }

    def plus[A](a: LazyParser[S, T, A], b: => LazyParser[S, T, A]) = LazyParser(M.plus(a.parser, b.parser))

    def empty[A] = LazyParser(M.empty[A])
  }

  implicit def lazyStateParserCommitment[S, T](implicit C: Commitment[({type l[a] = Parser[S, T, a]})#l]): Commitment[({type l[a]=LazyParser[S, T, a]})#l] = new Commitment[({type l[a]=LazyParser[S, T, a]})#l] {
    def commit[A](p: LazyParser[S, T, A]) = LazyParser(C.commit(p.parser))

    def oneOf[A](t: List[LazyParser[S, T, A]]) = LazyParser(C.oneOf(t.map(_.parser)))

    def fail[A](reason: FailureReason) = LazyParser(C.fail(reason))
  }

}

trait LazyParserFunctions {

  def next[S, T]: LazyParser[S, T, T] = LazyParser(Parser.next[S, T])
  def eof[S, T]: LazyParser[S, T, Unit] = LazyParser(Parser.eof[S, T])
  def satisfy[S, T](pred: T => Boolean)(implicit M: MonadPlus[({type l[a]=Parser[S, T, a]})#l]): LazyParser[S, T, T] = LazyParser(Parser.satisfy(pred))
  def onFail[S, T, A](pa: => LazyParser[S, T, A], pb: => LazyParser[S, T, A]): LazyParser[S, T, A] = LazyParser(Parser.onFail(pa.parser, pb.parser))

  def stUpdate[S, T](f: S => S): LazyParser[S, T, Unit] = LazyParser(Parser.stUpdate(f))
  def stQuery[S, T, A](f: S => A): LazyParser[S, T, A] = LazyParser(Parser.stQuery(f))
  def stGet[S, T]: LazyParser[S, T, S] = LazyParser(Parser.stGet)

  def reparse[S, T](ts: List[T]): LazyParser[S, T, Unit] = LazyParser(Parser.reparse(ts))
}

object LazyParser extends LazyParserFunctions with LazyParserInstances {
//  def apply[S, T, A](f: (S, List[T]) => Result[(List[T], S), A]): LazyParser[S, T, A] = LazyParser(Parser(f))

  def runParser[S, T, A](parser: LazyParser[S, T, A], state: S, tokens: List[T]): (Either[FailureReason, A], S, List[T]) = {
    val (either, (ts, s)) = resultToEither(parser(state, tokens))
    (either, s, ts)
  }
}
