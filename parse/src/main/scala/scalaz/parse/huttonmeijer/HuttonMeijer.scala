package scalaz
package parse
package huttonmeijer


import std.list._

object Parser {
  def apply[A](f: Tokens => List[(A, Tokens)]): Parser[A] = new Parser[A] {
    def apply(ts: Tokens) = f(ts)
  }
}

trait Parser[A] {
  def apply(ts: Tokens): List[(A, Tokens)]
  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser(input=>
    apply(input).flatMap {
      case (v, out) => f(v).apply(out)
    }
  )
  def map[B](f: A => B): Parser[B] = Parser(input =>
    apply(input).map { case (v, out) => (f(v), out)}
  )
}

trait HuttonMeijerFunctions {
  self: HuttonMeijerInstances2 =>

  def item: Parser[Token] = Parser(input => input match {
    case Nil => Nil
    case x :: xs => List((x, xs))
  })

  def first[A](pa: => Parser[A]): Parser[A] = Parser(input => pa(input) match {
    case Nil => Nil
    case (x :: _) => List(x)
  })

  def +++[A](pa: => Parser[A], pb: => Parser[A]): Parser[A] = first(MonadPlus[Parser].plus(pa, pb))

  def sat(pred: Token => Boolean): Parser[Token] = for {
    x <- item
    p <- if(pred(x)) Monad[Parser].pure(x) else MonadPlus[Parser].empty
  } yield p

  def many[A](pa: => Parser[A]): Parser[List[A]] = +++(many1(pa), Monad[Parser].pure(Nil))

  def many1[A](pa: => Parser[A]): Parser[List[A]] = for {
    x <- pa
    xs <- many(pa)
  } yield x :: xs

  def sepBy[A, B](pa: => Parser[A], sep: => Parser[B]): Parser[List[A]] =
    +++(sepBy1(pa, sep), Monad[Parser].pure(Nil))

  def sepBy1[A, B](pa: => Parser[A], sep: => Parser[B]): Parser[List[A]] = for {
    x <- pa
    xs <- many(for {
      _ <- sep
      y <- pa
    } yield y)
  } yield x :: xs

  def chainl[A](pa: => Parser[A], op: => Parser[(A, A) => A], v: A): Parser[A] = +++(chainl1(pa, op), Monad[Parser].pure(v))

  def chainl1[A](pa: => Parser[A], op: => Parser[(A, A) => A]): Parser[A] = {
    def rest(x: A): Parser[A] =
      +++((op.flatMap { f =>
        pa.flatMap { y =>
          rest(f(x, y))
        }
      }), Monad[Parser].pure(x))

    for {
      x <- pa
      y <- rest(x)
    } yield y
  }

  def chainr[A](pa: => Parser[A], op: => Parser[(A, A) => A], v: A): Parser[A] = +++(chainr1(pa, op), Monad[Parser].pure(v))

  def chainr1[A](pa: => Parser[A], op: => Parser[(A, A) => A]): Parser[A] = {
    def rest(x: A): Parser[A] = {
      val rest_ = for {
        f <- op
        y <- chainr1(pa, op)
      } yield f(x, y)
      +++(rest_, Monad[Parser].pure(x))
    }

    for {
      x <- pa
      y <- rest(x)
    } yield y
  }

  def ops[A, B](xs: List[(Parser[A], B)]): Parser[B] = {
    val start = xs.map {
      case (p, op) => for {
        _ <- p
        op <- Monad[Parser].pure(op)
      } yield op
    }
    Foldable[List].foldr1(start)((a, b) => +++(a, b)).getOrElse(sys.error("bla"))
  }

  def bracket[A, B, C](open: Parser[A], pb: Parser[B], close: Parser[C]): Parser[B] = {
    for {
      _ <- open
      x <- pb
      _ <- close
    } yield x
  }


  def char(c: Char): Parser[Char] = sat(_ == c)
  def digit: Parser[Char] = sat(_.isDigit)
  def lower: Parser[Char] = sat(_.isLower)
  def upper: Parser[Char] = sat(_.isUpper)
  def letter: Parser[Char] = sat(_.isLetter)
  def alphanum: Parser[Char] = sat(_.isLetterOrDigit)
  def string(cs: List[Char]): Parser[List[Char]] = cs match {
    case Nil => Monad[Parser].pure(Nil)
    case s @ x :: xs => for {
      _ <- char(x)
      _ <- string(xs)
    } yield s
  }
  def ident: Parser[List[Char]] = for {
    x <- lower
    xs <- many(alphanum)
  } yield x :: xs

  def nat: Parser[Int] = {
    def op(a: Int, b: Int) = a*10 + b
    lazy val readInt = for {
      x <- digit
    } yield x.toInt - '0'.toInt
    chainl1[Int](readInt, Monad[Parser].pure(op(_, _)))
  }

  def int: Parser[Int] = {
    val nat0 = for {
      _ <- char('-')
      n <- nat
    } yield -n
    +++(nat0, nat)
  }

  def spaces: Parser[Unit] = many1(sat(_.isSpaceChar)).map(_ => ())
  def comment: Parser[Unit] = bracket(string("/*".toList), many(item), string("*/".toList)).map(_ => ())
  def junk: Parser[Unit] = many(+++(spaces, comment)).map(_ => ())
  def skip[A](pa: Parser[A]) = junk.flatMap(_ => pa)
  def token[A](pa: Parser[A]) = for {
    v <- pa
    _ <- junk
  } yield v

  def natural: Parser[Int] = token(nat)
  def integer: Parser[Int] = token(int)
  def symbol(s: List[Char]): Parser[List[Char]] = token(string(s))
  def identifier(ss: List[List[Char]]): Parser[List[Char]] ={
    val idents = ident.flatMap(x => if(!ss.contains(x)) Monad[Parser].pure(x) else MonadPlus[Parser].empty[List[Char]])
    token(idents)
  }

}

trait HuttonMeijerInstances2 {
  self: HuttonMeijerFunctions =>

  implicit def hmParserFunctor[A]: Functor[Parser] = new Functor[Parser] {
    def map[A, B](fa: Parser[A])(f: (A) => B) = fa.map(f)
  }

  implicit def hmParserMonad[A]: MonadPlus[Parser] = new MonadPlus[Parser] {
    def bind[A, B](fa: Parser[A])(f: (A) => Parser[B]) = fa.flatMap(f)

    def point[A](a: => A) = Parser(input => List((a, input)))

    def plus[A](a: Parser[A], b: => Parser[A]) = Parser(input => a(input) ++ b(input))

    def empty[A] = Parser(_ => Nil)
  }
}

object HuttonMeijer extends HuttonMeijerFunctions with HuttonMeijerInstances2
