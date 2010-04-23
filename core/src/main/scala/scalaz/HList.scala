package scalaz


sealed trait HList {
  type Wrap[M[_]] <: HList
  type Function[B]
  type Fold[C[_, _ <: HList] <: Up, N <: Up, Up] <: Up
}

final class HNil extends HList {
  type Wrap[M[_]] = HNil
  type Function[B] = B
  def ::[T](v: T) = HCons(v, this)
  type Fold[C[_, _ <: HList] <: Up, N <: Up, Up] = N
  type H = Nothing
  type T = HNil
  def fold[C[_, _ <: HList] <: Up, N <: Up, Up](f: (H, T) => C[H, T], g: => N): Fold[C, N, Up] = g
}

final case class HCons[Head, Tail <: HList](head: Head, tail: Tail) extends HList {
  import HList._

  type Wrap[M[_]] = M[H] :: T#Wrap[M]

  type Function[B] = H => T#Function[B] 

  def ::[T](v: T) = HCons(v, this)
  
  type Fold[C[_, _ <: HList] <: Up, N <: Up, Up] = C[H, T]

  type H = Head
  type T = Tail

  def fold[C[_, _ <: HList] <: Up, N <: Up, Up](f: (H, T) => C[H, T], g: => N): Fold[C, N, Up] = f(head, tail)
}

trait HLists {
  val HNil = new HNil()

  type ::[H, T <: HList] = HCons[H, T]
}

object HList extends HLists
