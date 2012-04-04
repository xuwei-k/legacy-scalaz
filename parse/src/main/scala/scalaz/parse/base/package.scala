package scalaz
package parse

package object base {

  def discard[A, B, F[_]](fa: => F[A], fb: => F[B])(implicit P: PolyParse[F]): F[A] = {
    import P.M
    M.bind(fa)(a => M.map(fb)(_ => a))
  }

  def failBad[A, F[_]](e: FailureReason)(implicit P: PolyParse[F]): F[A] = P.C.commit(P.C.fail(e))

  def oneOf[A, F[_]](fas: List[F[A]])(implicit P: PolyParse[F]): F[A] = fas match {
    case Nil => P.C.fail(NoSuppliedParserSucceeded)
    case (f :: fs) => P.M.plus(f, oneOf(fs))
  }

  def exactly[A, F[_]](fa: F[A], i: Int)(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    if (i == 0) M.pure(List())
    else {
      val err = orFail(fa, MoreItemsExpected(i))
      val apl = M.ap(err)(M.pure((a: A) => a :: (_: List[A])))
      M.ap(exactly(fa, i-1))(apl)
    }
  }

  def upto[A, F[_]](fa: F[A], n: Int)(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    if (n == 0) M.pure(List.empty[A])
    else {
      M.bind(fa) { a =>
        M.plus(
          M.ap(upto(fa, n-1))(M.pure(a :: (_: List[A]))),
          M.pure(List.empty[A])
        )
      }
    }
  }

  def orFail[A, F[_]](fa: F[A], e: FailureReason)(implicit P: PolyParse[F]): F[A] = {
    P.M.plus(fa, P.C.fail(e))
  }

  def many1[A, F[_]](fa: F[A])(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    M.bind(fa) { a =>
      M.ap(M.many(fa))(M.pure(a :: (_: List[A])))
    }
  }

  def sepBy[A, SEP, F[_]](fa: => F[A], sep: => F[SEP])(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    M.map(M.plus(sepBy1(fa, sep), M.pure(List.empty[A])))(identity)
  }

  def sepBy1[A, SEP, F[_]](fa: => F[A], sep: => F[SEP])(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    lazy val nested = M.many(M.bind(sep)(_ => fa))
    M.bind(fa) { a =>
      M.ap(nested)(M.pure(a :: (_: List[A])))
    }
  }

  def bracketSep[A, BRA, KET, SEP, F[_]](fa: => F[A])(open: => F[BRA], sep: => F[SEP], close: => F[KET])(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    lazy val left = M.bind(open)(_ => M.bind(close)(_ => M.pure(List.empty[A])))
    lazy val right = {
      M.bind(open) { _ =>
        M.bind(fa) { x =>
          M.ap(manyFinally(M.bind(sep)(_ => M.map(fa)(identity)), close))(M.pure(x :: (_: List[A])))
        }
      }
    }
    M.plus(left, right)
  }

  def bracket[A, BRA, KET, F[_]](fa: => F[A], open: => F[BRA], close: => F[KET])(implicit P: PolyParse[F]): F[A] = {
    import P._
    M.bind(orFail(open, MissingOpeningBracket)) { _ =>
      M.map(discard(fa, orFail(close, MissingClosingBracket)))(identity)
    }
  }

  def manyFinally[A, Z, F[_]](fa: => F[A], terminator: => F[Z])(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    M.plus(
      discard(M.many(fa), terminator),
      C.oneOf(
        List(
          M.bind(fa)(_ => M.pure(List.empty[A])),
          M.bind(terminator)(_ => M.pure(List.empty[A]))
        )
      )
    )
  }

  def manyFinally_[A, Z, F[_]](fa: => F[A], fz: => F[Z])(implicit P: PolyParse[F]): F[List[A]] = {
    import P._
    M.plus(
      M.map(fz)(_ => List.empty[A]),
      M.plus(
        M.map2(fa, manyFinally_(fa, fz))(_ :: _),
        C.oneOf(
          List(
            M.bind(fz)(_ => M.pure(List.empty[A])),
            M.bind(fa)(_ => M.pure(List.empty[A]))
          )
        )
      )
    )
  }
}