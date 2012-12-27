package scalaz

/**
 * Partial Lens Families, offering a purely functional means to access and retrieve
 * an optional field transitioning from type `B1` to type `B2` in a record that is
 * simultaneously transitioning from type `A1` to type `A2`.  [[scalaz.PLens]] is a
 * convenient alias for when `F =:= Id`, `A1 =:= A2`, and `B1 =:= B2`.
 *
 * The term ''field'' should not be interpreted restrictively to mean a member of a class. For example, a partial lens
 * family can address the nth element of a `List`.
 *
 * @see [[scalaz.Lens]]
 *
 * @tparam A1 The initial type of the record
 * @tparam A2 The final type of the record
 * @tparam B1 The initial type of the optional field
 * @tparam B2 The final type of the optional field
 */
sealed trait PLeensFamily[-A1, +A2, +B1, -B2] {
  def run(a: A1): Option[IndexedStore[B1, B2, A2]]

  def apply(a: A1): Option[IndexedStore[B1, B2, A2]] =
    run(a)

  /*

  import StateT._


   */
  import BijectionT._
  import PLeensFamily._

  def kleisli: Kleisli[Option, A1, IndexedStore[B1, B2, A2]] =
    Kleisli[Option, A1, IndexedStore[B1, B2, A2]](apply(_))

  def xmapA[X1, X2](f: A2 => X2)(g: X1 => A1): PLeensFamily[X1, X2, B1, B2] =
    plensFamily(x => apply(g(x)) map (_ map (f)))

  def xmapbA[X, A >: A2 <: A1](b: Bijection[A, X]): PLeensFamily[X, X, B1, B2] =
    xmapA(b to _)(b from _)

  def xmapB[X1, X2](f: B1 => X1)(g: X2 => B2): PLeensFamily[A1, A2, X1, X2] =
    plensFamily(a => apply(a) map (_.xmap(f)(g)))

  def xmapbB[X, B >: B1 <: B2](b: Bijection[B, X]): PLeensFamily[A1, A2, X, X] =
    xmapB(b to _)(b from _)

  def get(a: A1): Option[B1] =
    run(a) map (_.pos)

  def getK: Kleisli[Option, A1, B1] =
    Kleisli[Option, A1, B1](get(_))

  /** If the Partial Lens is null, then return the given default value. */
  def getOr[B >: B1](a: A1, b: => B): B =
    get(a) getOrElse b

  def getOrZ[B >: B1](a: A1)(implicit M: Monoid[B]): B =
    getOr(a, M.zero)

  def set(a: A1, b: B2): Option[A2] =
    run(a) map (_.put(b))

  def setK(a: A1): Kleisli[Option, B2, A2] =
    Kleisli[Option, B2, A2](set(a, _))

  def setOr[A >: A2](a: A1, b: B2, d: => A): A =
    set(a, b) getOrElse d

  def setOrZ[A >: A2](a: A1, b: B2)(implicit M: Monoid[A]): A =
    setOr(a, b, M.zero)

  def trySet(a: A1): Option[B2 => A2] =
    run(a) map (c => c put _)

  def trySetK: Kleisli[Option, A1, B2 => A2] =
    Kleisli[Option, A1, B2 => A2](trySet(_))

  def trySetOr[A >: A2, B <: B2](a: A1, d: => B => A): B => A =
    trySet(a) getOrElse d

  def trySetOrZ[A >: A2, B <: B2](a: A1)(implicit M: Monoid[B => A]): B => A =
    trySetOr(a, M.zero)

  /** If the Partial Lens is null, then return the target object, otherwise run the function on its projection. */
  def as[A <: A1](f: B1 => A, a: A): A =
    get(a) match {
      case None => a
      case Some(w) => f(w)
    }

  def is(a: A1): Boolean =
    run(a).isDefined

  def isNot(a: A1): Boolean =
    !is(a)

  def exists(p: B1 => Boolean, a: A1): Boolean =
    get(a) exists p

  def forall(p: B1 => Boolean, a: A1): Boolean =
    get(a) forall p

  def modg(f: B1 => B2, a: A1): Option[A2] =
    run(a).map(_ puts f)

  def =?>=(f: B1 => B2): A1 => Option[A2] =
    modg(f, _)

  /** Modify the potential value viewed through the partial lens */
  def mod[A >: A2 <: A1](f: B1 => B2, a: A): A =
    run(a) match {
      case None => a: A
      case Some(w) => (w puts f): A
    }

  def =>=[A >: A2 <: A1](f: B1 => B2): A => A =
    mod(f, _)

  def st[A <: A1]: PState[A, B1] =
    State(s => (s, get(s)))

  def %=[A >: A2 <: A1, B <: B2](f: B1 => B): PState[A, B] =
    State(a => run(a) match {
      case None => (a, None)
      case Some(w) => {
        val r = f(w.pos)
        (w put r, Some(r))
      }
    })

  def :=[A >: A2 <: A1, B <: B2](b: => B): PState[A, B] =
    %=(_ => b)

  def %==[A >: A2 <: A1, B <: B2](f: B1 => B): State[A, Unit] =
    State(a =>
  (mod(f, a), ()))

  def %%=[A >: A2 <: A1, C](s: IndexedState[B1, B2, C]): PState[A, C] =
    State(a => run(a) match {
      case None => (a, None)
      case Some(w) => {
        val r = s.run(w.pos): (B2, C)
        (w put r._1, Some(r._2))
      }
    })

  def >-[A >: A2 <: A1, C](f: B1 => C): PState[A, C] =
    State(a => (a, get(a) map f))

  def >>-[A >: A2 <: A1, C](f: B1 => State[A, C]): PState[A, C] =
    StateT(a => get(a) match {
      case None => (a, None)
      case Some(w) =>
        f(w) apply a match {
          case (y, x) => (y, Some(x))
        }
    })

  def ->>-[A >: A2 <: A1, C](f: => State[A, C]): PState[A, C] =
    >>-(_ => f)

  /** Partial Lenses can be composed */
  def compose[C1, C2](that: PLeensFamily[C1, C2, A1, A2]): PLeensFamily[C1, C2, B1, B2] =
    plensFamily(c =>
      (that run c).flatMap (x => {
        val (ac, a) = x.run
        run(a) map (y => {
          val (ba, b) = y.run
          IndexedStore(ac compose ba, b)
        })
      }))

  /** alias for `compose` */
  def <=<[C1, C2](that: PLeensFamily[C1, C2, A1, A2]): PLeensFamily[C1, C2, B1, B2] = compose(that)

  def andThen[C1, C2](that: PLeensFamily[B1, B2, C1, C2]): PLeensFamily[A1, A2, C1, C2] =
    that compose this

  /** alias for `andThen` */
  def >=>[C1, C2](that: PLeensFamily[B1, B2, C1, C2]): PLeensFamily[A1, A2, C1, C2] = andThen(that)

  /** Two partial lenses that view a value of the same type can be joined */
  def sum[C1, C2, B1m >: B1, B2m <: B2](that: => PLeensFamily[C1, C2, B1m, B2m]): PLeensFamily[A1 \/ C1, A2 \/ C2, B1m, B2m] =
    plensFamily {
      case -\/(a) =>
        run(a) map (_ map (-\/(_)))
      case \/-(c) =>
        that run c map (_ map (\/-(_)))
    }

  /** Alias for `sum` */
  def |||[C1, C2, B1m >: B1, B2m <: B2](that: => PLeensFamily[C1, C2, B1m, B2m]): PLeensFamily[A1 \/ C1, A2 \/ C2, B1m, B2m] = sum(that)

  /** Two disjoint partial lenses can be paired */
  def product[C1, C2, D1, D2](that: PLeensFamily[C1, C2, D1, D2]): PLeensFamily[(A1, C1), (A2, C2), (B1, D1), (B2, D2)] =
    plensFamily {
      case (a, c) =>
        for {
          q <- run(a)
          r <- that run c
        } yield q *** r
    }

  /** alias for `product` */
  def ***[C1, C2, D1, D2](that: PLeensFamily[C1, C2, D1, D2]): PLeensFamily[(A1, C1), (A2, C2), (B1, D1), (B2, D2)] = product(that)

}

object PLeensFamily extends PLeensFunctions with PLeensInstances {
  /*
  def apply[F[+_], A1, A2, B1, B2](r: A1 => F[Option[IndexedStore[B1, B2, A2]]]): PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilyT(r)
    */
}

trait PLeensFamilyFunctions extends PLeensInstances {

  def plensFamily[A1, A2, B1, B2](r: A1 => Option[IndexedStore[B1, B2, A2]]): PLeensFamily[A1, A2, B1, B2] = new PLeensFamily[A1, A2, B1, B2] {
    def run(a: A1): Option[IndexedStore[B1, B2, A2]] = r(a)
  }
}

trait PLeensFunctions extends PLeensFamilyFunctions with PLeensInstances {

}

trait PLeensInstance0 {

}

trait PLeensInstances extends PLeensInstance0 {
}