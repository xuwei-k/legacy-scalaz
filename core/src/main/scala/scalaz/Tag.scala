package scalaz

object Tag {
  // TODO According to Miles, @specialized doesn't help here. Maybe manually specialize.
  @inline def apply[@specialized A, T](a: A): A @@ T = a.asInstanceOf[A @@ T]

  def subst[A, F[_], T](fa: F[A]): F[A @@ T] = fa.asInstanceOf[F[A @@ T]]

  def unsubst[A, F[_], T](fa: F[A @@ T]): F[A] = fa.asInstanceOf[F[A]]
  def unsubstT[A[_], F[_], T](fa: F[A[_] @@ T]): F[A[_]] = fa.asInstanceOf[F[A[_]]]
}
