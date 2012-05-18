package scalaz
package parse
package base

trait PolyParse[F[_]] {
  def C: Commitment[F]
  def M: MonadPlus[F]
}

object PolyParse {
  def apply[F[_]](implicit _C: Commitment[F], _M: MonadPlus[F]) = new PolyParse[F] {
    def C = _C
    def M = _M
  }

  implicit def polyParseInstance[F[_] : MonadPlus : Commitment]: PolyParse[F] = PolyParse[F]
}
