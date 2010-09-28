package scalaz

sealed trait MAB[M[_, _], A, B] extends PimpedType[M[A, B]] {
  def :->[D](g: B => D)(implicit b: Bifunctor[M]): M[A, D] = b.bimap(value, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]): M[C, B] = b.bimap(value, f, identity[B])

  def >>>[C](k: M[B, C])(implicit c: Category[M]): M[A, C] = c compose (k, value)
 
  def ⋙[C](k: M[B, C])(implicit c: Category[M]): M[A, C] = c compose (k, value)

  def <<<[C](k: M[C, A])(implicit c: Category[M]): M[C, B] = c compose (value, k)
 
  def ⋘[C](k: M[C, A])(implicit c: Category[M]): M[C, B] = c compose (value, k)

  def first[C](implicit a: Arrow[M]): M[(A, C), (B, C)] = a first value

  def second[C](implicit a: Arrow[M]): M[(C, A), (C, B)] = a second value

  def ***[C, D](k: M[C, D])(implicit a: Arrow[M]): M[(A, C), (B, D)] = a.category.compose(a.second[C, D, B](k), first[C])

  def &&&[C](k: M[A, C])(implicit a: Arrow[M]): M[A, (B, C)] = a.category.compose(***(k), a.arrow(a => (a, a)))

  def product(implicit a: Arrow[M]): M[(A, A), (B, B)] = this *** value

  def ^>>[C](f: C => A)(implicit a: Arrow[M]): M[C, B] = a.category.compose(value, a.arrow(f))

  def >>^[C](f: B => C)(implicit a: Arrow[M]): M[A, C] = a.category.compose(a.arrow(f), value)

  def <<^[C](f: C => A)(implicit a: Arrow[M]): M[C, B] = a.category.compose(value, a.arrow(f))

  def ^<<[C](f: B => C)(implicit a: Arrow[M]): M[A, C] = a.category.compose(a.arrow(f), value)
}

trait MABs {
  implicit def mab[M[_, _], A, B](a: M[A, B]): MAB[M, A, B] = new MAB[M, A, B] {
    val value = a
  }
}

object MAB extends MABs {
  // move to MABs once https://lampsvn.epfl.ch/trac/scala/ticket/2741 is solved.
  implicit def KleisliMAB[M[_], A, B](k: Kleisli[M, A, B]): MAB[PartialApplyK[Kleisli, M]#Apply, A, B] = mab[PartialApplyK[Kleisli, M]#Apply, A, B](k)

  implicit def CokleisliMAB[M[_], A, B](k: Cokleisli[M, A, B]): MAB[PartialApplyK[Cokleisli, M]#Apply, A, B] = mab[PartialApplyK[Cokleisli, M]#Apply, A, B](k)

  implicit def Const2MAB[M, A, B](k: Const2[M,A,B]): MAB[PartialApply1Of3[Const2,M]#Apply, A, B] =
    mab[PartialApply1Of3[Const2, M]#Apply, A, B](k)

}