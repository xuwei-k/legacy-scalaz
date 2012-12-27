package scalaz

import StoreT._
import Id._

/**
 * A Lens Family, offering a purely functional means to access and retrieve
 * a field transitioning from type `B1` to type `B2` in a record simultaneously
 * transitioning from type `A1` to type `A2`.  [[scalaz.Lens]] is a convenient
 * alias for when `F =:= Id`, `A1 =:= A2`, and `B1 =:= B2`.
 *
 * The term ''field'' should not be interpreted restrictively to mean a member of a class. For example, a lens
 * family can address membership of a `Set`.
 *
 * @see [[scalaz.PLens]]
 *
 * @tparam A1 The initial type of the record
 * @tparam A2 The final type of the record
 * @tparam B1 The initial type of the field
 * @tparam B2 The final type of the field
 */
sealed trait LeensFamily[-A1, +A2, +B1, -B2] {
  def run(a: A1): IndexedStore[B1, B2, A2]

  def apply(a: A1): IndexedStore[B1, B2, A2] =
    run(a)

  import LeensFamily._
  import BijectionT._

  def xmapA[X1, X2](f: A2 => X2)(g: X1 => A1): LeensFamily[X1, X2, B1, B2] =
    lensFamily(x => run(g(x)) map f)

  def xmapbA[X, A >: A2 <: A1](b: Bijection[A, X]): LeensFamily[X, X, B1, B2] =
    xmapA(b to _)(b from _)

  def xmapB[X1, X2](f: B1 => X1)(g: X2 => B2): LeensFamily[A1, A2, X1, X2] =
    lensFamily(a => run(a).xmap(f)(g))

  def xmapbB[X, B >: B1 <: B2](b: Bijection[B, X]): LeensFamily[A1, A2, X, X] =
    xmapB(b to _)(b from _)

  def get(a: A1): B1 =
    run(a).pos

  def set(a: A1, b: B2): A2 =
    run(a).put(b)

  def st[A <: A1]: State[A, B1] =
    State(s => (s, get(s)))

  /** Modify the value viewed through the lens */
  def mod(f: B1 => B2, a: A1): A2 = {
    val (p, q) = run(a).run
    p(f(q))
  }

  def =>=(f: B1 => B2): A1 => A2 =
    mod(f, _)

  /** Modify the value viewed through the lens, returning a functor `X` full of results. */
  def modf[X[+_]](f: B1 => X[B2], a: A1)(implicit XF: Functor[X]): X[A2] = {
    val c = run(a)
    XF.map(f(c.pos))(c put _)
  }

  def =>>=[X[+_]](f: B1 => X[B2])(implicit XF: Functor[X]): A1 => X[A2] =
    modf(f, _)

  /** Modify the value viewed through the lens, returning a `C` on the side.  */
  def modp[C](f: B1 => (B2, C), a: A1): (A2, C) = {
    val (b, c) = f(get(a))
    (set(a, b), c)
  }

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def mods[B <: B2](f: B1 => B): IndexedState[A1, A2, B] =
    IndexedState(a => {
      val c = run(a)
      val b = f(c.pos)
      (c put b, b)
    })

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def %=[B <: B2](f: B1 => B): IndexedState[A1, A2, B] =
    mods[B](f)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def assign[B <: B2](b: => B): IndexedState[A1, A2, B] =
    mods[B](_ => b)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def :=[B <: B2](b: => B): IndexedState[A1, A2, B] =
    assign[B](b)

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def mods_(f: B1 => B2): IndexedState[A1, A2, Unit] =
    IndexedState(a =>
      (mod(f, a), ()))

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def %==(f: B1 => B2): IndexedState[A1, A2, Unit] =
    mods_(f)



}

object LeensFamily extends LeensFunctions with LeensInstances {
  def apply[A1, A2, B1, B2](r: A1 => IndexedStore[B1, B2, A2]): LeensFamily[A1, A2, B1, B2] =
    lensFamily(r)
}

trait LeensFamilyFunctions {
  import StoreT._

  def lensFamily[A1, A2, B1, B2](r: A1 => IndexedStore[B1, B2, A2]): LeensFamily[A1, A2, B1, B2] = new LeensFamily[A1, A2, B1, B2] {
    def run(a: A1): IndexedStore[B1, B2, A2] = r(a)
  }
}

trait LeensFunctions extends LeensFamilyFunctions {

}

trait LeensInstances0 { this: LeensInstances =>

}

trait LeensInstances extends LeensInstances0 {

}
