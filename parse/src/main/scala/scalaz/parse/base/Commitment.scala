package scalaz
package parse
package base

trait Commitment[F[_]] {
  def commit[A](fa: F[A]): F[A]

  def fail[A](reason: FailureReason): F[A]

  def oneOf[A](t: List[F[A]]): F[A]
}

object Commitment {
  def apply[F[_]: Commitment]: Commitment[F] = implicitly[Commitment[F]]
}