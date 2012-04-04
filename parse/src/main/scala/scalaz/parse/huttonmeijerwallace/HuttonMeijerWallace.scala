package scalaz
package parse
package huttonmeijerwallace


import std.list._
import base._


// type ParseResult[S, T, E, A] = Either[E, List[(A, S, List[Either[E, T]])]]


abstract class Parser[S, T, E, A] {
  def apply(st: S, inp: List[Either[E, T]]): ParseResult[S, T, E, A]

  import Parser._

  def map[B](f: (A) => B): Parser[S, T, E, B] = Parser((st, inp) => this(st, inp) match {
    case Right(res) => Right(res.map { case (v, st, out) => (f(v), st, out)})
    case Left(err) => Left(err)
  })

  def flatMap[B](f: (A) => Parser[S, T, E, B]): Parser[S, T, E, B] = Parser((st, inp) => this(st, inp) match {
    case Right(res) =>
      val applied = res.map { case (v, st, out) => applyParser(f(v), st, out) }
      Foldable[List].foldR(applied, Right(Nil): ParseResult[S, T, E, B])((joinResults[S, T, E, B] _).curried)
    case Left(err) => Left(err)
  })

  def +++(pb: Parser[S, T, E, A]): Parser[S, T, E, A] = first(MonadPlus[({type λ[α]=Parser[S, T, E, α]})#λ].plus(this, pb))
}

object Parser extends ParserInstances with ParserFunctions{
  def apply[S, T, E, A](f: (S, List[Either[E, T]]) => ParseResult[S, T, E, A]): Parser[S, T, E, A] = new Parser[S, T, E, A] {
    def apply(st: S, inp: List[Either[E, T]]) = f(st, inp)
  }
}



trait ParserInstances {
  implicit def parserMonadPlus[S, T, E]: MonadPlus[({type λ[α]=Parser[S, T, E, α]})#λ] = new MonadPlus[({type λ[α]=Parser[S, T, E, α]})#λ] {
    override def map[A, B](fa: Parser[S, T, E, A])(f: (A) => B): Parser[S, T, E, B] = fa.map(f)

    def bind[A, B](fa: Parser[S, T, E, A])(f: (A) => Parser[S, T, E, B]): Parser[S, T, E, B] = fa.flatMap(f)

    def point[A](a: => A): Parser[S, T, E, A] = Parser((st, inp) => Right(((a, st, inp)) :: Nil))

    def empty[A]: Parser[S, T, E, A] = Parser((st, inp) => Right(Nil))

    def plus[A](a: Parser[S, T, E, A], b: => Parser[S, T, E, A]) = Parser((st, inp) => Parser.joinResults(a(st, inp), b(st, inp)))
  }
}

trait ParserFunctions {

  def +++[S, T, E, A](pa: Parser[S, T, E, A], pb: => Parser[S, T, E, A]): Parser[S, T, E, A] = first(MonadPlus[({type λ[α]=Parser[S, T, E, α]})#λ].plus(pa, pb))


  def joinResults[S, T, E, A](a: ParseResult[S, T, E, A], b: => ParseResult[S, T, E, A]): ParseResult[S, T, E, A] = a match {
    case Left(err) => Left(err)
    case Right(Nil) => b
    case Right(as) => Right(as ::: b.fold(fa = _ => Nil, fb = identity))
  }

  def applyParser[S, T, E, A](parser: Parser[S, T, E, A], st: S, inp: List[Either[E, T]]): ParseResult[S, T, E, A] = parser(st, inp)

  def item[S, T, E]: Parser[S, T, E, T] = Parser((st, inp) => inp match {
    case Nil => Right(Nil)
    case Left(err) :: _ => Left(err)
    case Right(x) :: xs => Right(List((x, st, xs)))
  })

  def eof[S, T, P]: Parser[S, T, FailureReason, Unit] = Parser((st, inp) => inp match {
    case Nil => Right(List(((), st, Nil)))
    case Left(e) :: _ => Left(e)
    case Right(t) :: _ => Left(UnexpectedInput(t))
  })

  def first[S, T, E, A](parser: Parser[S, T, E, A]): Parser[S, T, E, A] = Parser((st, inp) => parser(st, inp) match {
    case Right(x :: _) => Right(List(x))
    case otherwise => otherwise
  })

  def satisfy[S, T, E, P](pred: T => Boolean)(implicit M: MonadPlus[({type λ[α]=Parser[S, T, E, α]})#λ]): Parser[S, T, E, T] =
    item[S, T, E].flatMap(x => if (pred(x)) M.point(x) else M.empty)

def token[S, T, E, P](t: T)(implicit E: Equal[T], M: MonadPlus[({type λ[α]=Parser[S, T, E, α]})#λ]): Parser[S, T, E, T] = satisfy(E.equal(_, t))

  def notToken[S, T, E, P](ts: List[T])(implicit E: Equal[T], M: MonadPlus[({type λ[α]=Parser[S, T, E, α]})#λ]): Parser[S, T, E, T] = satisfy(!ts.contains(_))

  def many[S, T, E, A](p: Parser[S, T, E, A]): Parser[S, T, E, List[A]] = many1(p)

  def many1[S, T, E, A](p: Parser[S, T, E, A]): Parser[S, T, E, List[A]] = {
    for {
      x <- p
      xs <- many(p)
    } yield x :: xs
  }

  def sepBy[S, T, E, A, B](p: Parser[S, T, E, A], sep: Parser[S, T, E, B])(implicit M: Monad[({type λ[α]=Parser[S, T, E, α]})#λ]): Parser[S, T, E, List[A]] = {
    sepBy1(p, sep) +++ M.point(Nil)
  }

  def sepBy1[S, T, E, A, B](p: Parser[S, T, E, A], sep: Parser[S, T, E, B]): Parser[S, T, E, List[A]] = {
    for {
      x <- p
      xs <- many(sep.flatMap(_ => p))
    } yield x :: xs
  }

  def chainl[S, T, E, A](p: Parser[S, T, E, A], op: Parser[S, T, E, (A, A) => A], a: A)(implicit M: Monad[({type λ[α]=Parser[S, T, E, α]})#λ]): Parser[S, T, E, A] = {
    chainl1(p, op) +++ M.point(a)
  }

  def chainl1[S, T, E, A](p: Parser[S, T, E, A], op: Parser[S, T, E, (A, A) => A]): Parser[S, T, E, A] = {
    def rest(x: A): Parser[S, T, E, A] = for {
      f <- op
      y <- p
      z <- rest(f(x, y))
    } yield z

    for {
      x <- p
      r <- rest(x)
    } yield x
  }

  def chainr[S, T, E, A](p: Parser[S, T, E, A], op: Parser[S, T, E, (A, A) => A], a: A)(implicit M: Monad[({type λ[α]=Parser[S, T, E, α]})#λ]): Parser[S, T, E, A] = {
    chainr1(p, op) +++ M.point(a)
  }

  def chainr1[S, T, E, A](p: Parser[S, T, E, A], op: Parser[S, T, E, (A, A) => A])(implicit M: Monad[({type λ[α]=Parser[S, T, E, α]})#λ]): Parser[S, T, E, A] = {
    def rest(x: A): Parser[S, T, E, A] = (for {
      f <- op
      y <- chainr1(p, op)
      z <- rest(f(x, y))
    } yield z) +++ M.point(x)

    for {
      x <- p
      r <- rest(x)
    } yield r
  }

  def ops[S, T, E, A, B](ps: List[(Parser[S, T, E, A], B)]): Option[Parser[S, T, E, B]] = {
    val opped = for {
      (p, op) <- ps
    } yield for {
        _ <- p
      } yield op

    Foldable[List].foldR1(opped)((+++[S, T, E, B] _).curried)
  }

  def bracket[S, T, E, OPEN, A, CLOSE, P](open: Parser[S, T, E, OPEN], p: Parser[S, T, E, A], close: Parser[S, T, E, CLOSE]): Parser[S, T, E, A] = {
    for {
      _ <- open //parseOrError(open, MissingOpeningBracket)
      x <- p
      _ <- close //parseOrError(close, MissingClosingBracket)
    } yield x
  }

  def toEOF[S, T, A, P](p: Parser[S, T, FailureReason, A]): Parser[S, T, FailureReason, A] = {
    for {
      x <- p
      _ <- eof
    } yield x
  }


  def reparse[S, T, E](input: List[Either[E, T]]): Parser[S, T, E, Unit] = Parser((st, inp) => Right(((), st, input ++ inp) :: Nil))


  //===================================================================
  // Error handling
  //===================================================================
  def parseError[S, T, A, P](reason: FailureReason): Parser[S, T, FailureReason, A] =
    Parser((st, inp) => inp match {
      case Nil => Left(UnexpectedEOF)
      case Left(e) :: _ => Left(e)
      case Right(t) :: _ => Left(FailureWithContext(t, reason))
    })


  def parseOrError[S, T, A, P](p: Parser[S, T, FailureReason, A], reason: FailureReason): Parser[S, T, FailureReason, A] = p +++ parseError(reason)

  //===================================================================
  // State handling
  //===================================================================

  def stUpdate[S, T, E](f: S => S): Parser[S, T, E, Unit] = Parser((st, inp) => Right(((), f(st), inp) :: Nil))

  def stQuery[S, T, E, A](f: S => A): Parser[S, T, E, A] = Parser((st, inp) => Right((f(st), st, inp) :: Nil))

  def stGet[S, T, E]: Parser[S, T, E, S] = Parser((st, inp) => Right((st, st, inp) :: Nil))

}