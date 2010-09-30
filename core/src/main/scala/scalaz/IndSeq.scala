package scalaz

// Indexed sequences
sealed trait IndSeq[A] extends NewType[FingerTree[Int, A]] {
  import MA._
  import IndSeq._

  val value: FingerTree[Int, A]

  implicit def sizer[A] = Reducer((a: A) => 1)

  def apply(i: Int): A =
    value.split(_ > i)._2.viewl.headOption.getOrElse(error("Index " + i + " > " + value.measure))

  def ++(xs: IndSeq[A]) = indSeq(value <++> xs.value)

  def :+(x: => A) = indSeq(value :+ x)

  def +:(x: => A) = indSeq(x +: value)

  def tail = indSeq(value.tail)

  def init = indSeq(value.init)

  def map[B](f: A => B) = indSeq(value map f)

  def flatMap[B](f: A => IndSeq[B]) =
    indSeq(value.foldl(FingerTree.empty[Int, B])((ys, x) => ys <++> f(x).value))
}

trait IndSeqs {
  private[scalaz] def indSeq[A](v: FingerTree[Int, A]) = new IndSeq[A] {
    val value = v
  }
}

object IndSeq extends IndSeqs {
  def apply[A](as: A*) = fromSeq(as)

  def fromSeq[A](as: Seq[A]) = indSeq(as.foldLeft(FingerTree.empty[Int, A](Reducer(a => 1)))((x, y) => x :+ y))
}