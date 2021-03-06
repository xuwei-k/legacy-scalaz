package scalaz

import annotation.tailrec

sealed trait Identity[A] extends PimpedType[A] {
  import Scalaz._

  def η[F[_]](implicit p: Pure[F]): F[A] = p pure value

  /**
   * Alias for {@link scalaz.Identity#η}
   */
  def pure[F[_]](implicit p: Pure[F]): F[A] = η

  def dual: Dual[A] = value

  def σ : Dual[A] = dual

  def ⊹(a: => A)(implicit s: Semigroup[A]): A = s append (value, a)

  /**
   * Alias for {@link scalaz.Identity#⊹}
   */
  def |+|(a: => A)(implicit s: Semigroup[A]): A = ⊹(a)

  def ≟(a: A)(implicit e: Equal[A]): Boolean = e equal (value, a)

  /**
   * Alias for {@link scalaz.Identity#≟}
   */
  def ===(a: A)(implicit e: Equal[A]): Boolean = ≟(a)

  def ≠(a: A)(implicit e: Equal[A]): Boolean = !(≟(a))

  /**
   * Alias for {@link scalaz.Identity#≠}
   */
  def /==(a: A)(implicit e: Equal[A]): Boolean = ≠(a)

  /**
   * Returns `a` if it is non-null, otherwise returns `d`.
   */
  def ??(d: => A)(implicit ev: Null <:< A): A = Option(value) getOrElse d

  // using the implicit parameter ev here gives better compiler error messages for mistyped expressions like  1 assert_≟ "".
  // the simpler signature is def assert_≟(b: A)(implicit e: Equal[A], s: Show[A])
  def assert_≟[B](b: B)(implicit e: Equal[A], s: Show[A], ev: B <:< A) = if (≠(b)) error(shows + " ≠ " + ev(b).shows)

  def ?|?(a: A)(implicit o: Order[A]): Ordering = o order (value, a)

  def ≤(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != GT

  def ≥(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != LT

  def ≨(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == LT

  def ≩(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == GT

  def ≮(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != LT

  def ≯(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != GT

  def ≰(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == GT

  def ≱(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == LT

  def lte(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != GT

  def gte(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != LT

  def lt(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == LT

  def gt(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == GT

  def min(a: A)(implicit o: Order[A]): A = if (lte(a)) value else a

  def max(a: A)(implicit o: Order[A]): A = if (gte(a)) value else a

  def show(implicit s: Show[A]): List[Char] = s.show(value)

  def shows(implicit s: Show[A]): String = s.show(value).mkString

  def print(implicit s: Show[A]): Unit = Console.print(shows)

  def println(implicit s: Show[A]): Unit = Console.println(shows)

  def mapply[F[_], B](f: F[A => B])(implicit ftr: Functor[F]): F[B] = f ∘ (_(value))

  def text(implicit s: Show[A]): xml.Text = xml.Text(value.shows)

  def <===>(a: A)(implicit m: MetricSpace[A]): Int = m distance (value, a)

  def constantState[S](s: => S): State[S, A] = Scalaz.state((_: S) => (s, value))

  def state[S]: State[S, A] = Scalaz.state((_: S, value))

  def unfold[M[_], B](f: A => Option[(B, A)])(implicit p: Pure[M], m: Monoid[M[B]]): M[B] = {
    @tailrec
    def unfold0(accum: M[B], v: Option[(B, A)]): M[B] = v match {
      case None => accum
      case Some((b, a)) => unfold0(accum ⊹ b.η, f(a))
    }

    unfold0(∅, f(value))
  }

  def replicate[M[_]](n: Int)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] = {
    @tailrec
    def replicate0(accum: M[A], n: Int): M[A] = if (n > 0) replicate0(accum ⊹ value.η, n - 1) else accum

    replicate0(∅, n)
  }

  def repeat[M[_]](implicit p: Pure[M], m: Monoid[M[A]]): M[A] = value.η ⊹ repeat

  def iterate[M[_]](f: A => A)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    value.η ⊹ f(value).iterate(f)

  def zipper: Zipper[A] = Scalaz.zipper(Stream.empty, value, Stream.empty)

  def unfoldTree[B](f: A => (B, () => Stream[A])): Tree[B] = f(value) match {
    case (a, bs) => node(a, bs.apply.unfoldForest(f))
  }

  def unfoldTreeM[B, M[_]](f: A => M[(B, Stream[A])])(implicit m: Monad[M]): M[Tree[B]] = {
    m.bind(f(value), (abs: (B, Stream[A])) =>
      m.bind(abs._2.unfoldForestM[B, M](f), (ts: Stream[Tree[B]]) =>
        m.pure(node(abs._1, ts))))
  }

  def success[X]: Validation[X, A] = Scalaz.success(value)

  def successNel[X]: ValidationNEL[X, A] = success

  def fail[X]: Validation[A, X] = failure(value)

  def failNel[X]: ValidationNEL[A, X] = failure(wrapNel)

  def some: Option[A] = Some(value)

  def pair: (A, A) = (value, value)

  def left[B]: Either[A, B] = Left(value)

  def right[B]: Either[B, A] = Right(value)

  def dlist: DList[A] = Scalaz.dlist(value :: (_: List[A]))

  def wrapNel: NonEmptyList[A] = Scalaz.nel1(value)

  @tailrec
  final def doWhile(f: A => A, p: A => Boolean): A = {
    val x = f(value)
    if (p(x)) x.doWhile(f, p) else x
  }

  @tailrec
  final def whileDo(f: A => A, p: A => Boolean): A =
    if (p(value)) f(value).whileDo(f, p) else value

  def squared: (A, A) = ×[A, A].μ apply value

  override def toString: String = value.toString

  override def hashCode: Int = value.hashCode

  override def equals(o: Any): Boolean = o != null && o.isInstanceOf[Identity[_]] && value == o.asInstanceOf[Identity[_]].value

  /** A pair lazy in its right value, with this value on the left and the given value on the right. **/
  def <&>[B](b: => B) = lazyTuple(value, b)

  /** Convert the value into a monoid **/
  def unit[M](implicit r: Reducer[A,M]) = r.unit(value)

  /** Convert the value into a monoid in a pointed functor **/
  def pureUnit[M[_], N](implicit m: Pure[M], r: Reducer[A,N]) = unit[N].pure

  /** Append the value to a monoid for use in left-to-right reduction **/ 
  def snoc[C](c: C)(implicit r: Reducer[C,A]) = r.snoc(value, c)

  /** Prepend the value to a monoid for use in right-to-left reduction **/
  def cons[M](m: M)(implicit r: Reducer[A,M]) = r.cons(value, m)
}

trait Identitys {
  implicit def IdentityTo[A](x: A): Identity[A] = new Identity[A] {
    val value = x
  }

  val unital = IdentityTo(())
}
