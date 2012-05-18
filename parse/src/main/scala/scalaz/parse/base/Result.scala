package scalaz
package parse
package base


sealed trait Result[Z, A] {
  def map[B](f: A => B): Result[Z, B]
}
case class ResultSuccess[Z, A](z: Z, a: A) extends Result[Z, A] {
  def map[B](f: (A) => B):ResultSuccess[Z, B] = ResultSuccess(z, f(a))
}
case class ResultFailure[Z, A](z: Z, e: FailureReason) extends Result[Z, A] {
  def map[B](f: (A) => B): ResultFailure[Z, B] = ResultFailure(z, e)
}
case class ResultCommitted[Z, A](result: Result[Z, A]) extends Result[Z, A] {
  def map[B](f: (A) => B): ResultCommitted[Z, B] = ResultCommitted(result.map(f))
}


trait ResultInstances {
  implicit def resultFunctor[Z] = new Functor[({type λ[α]=Result[Z, α]})#λ] {
    def map[A, B](fa: Result[Z, A])(f: (A) => B): Result[Z, B] = fa.map(f)
  }
}

trait ResultFunctions {
  def resultToEither[Z, A](r: Result[Z, A]): (Either[FailureReason, A], Z) = r match {
    case ResultSuccess(z, a) => (Right(a), z)
    case ResultFailure(z, e) => (Left(e), z)
    case ResultCommitted(res) => resultToEither(res)
  }
}


object Result extends ResultInstances with ResultFunctions