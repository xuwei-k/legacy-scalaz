package scalaz

// Ordered sequences
sealed trait OrdSeq[A] extends NewType[FingerTree[Option[A], A]] {
  import OrdSeq._
  import MAB._
  import Identity._
  import OptionW._

  val value: FingerTree[Option[A], A]
  implicit val ord: Order[A]

  def partition(a: A) = (ordSeq[A](_)).product.apply(value.split(_ gte some(a)))

  def insert(a: A) = partition(a) match {
    case (l, r) => ordSeq(l <++> (a +: r))
  }

  def ++(xs: OrdSeq[A]) = xs.toList.foldLeft(this)(_ insert _)
}

trait OrdSeqs {
  import OptionW._
  import Empty._
  
  private[scalaz] def ordSeq[A: Order](t: FingerTree[Option[A], A]) = new OrdSeq[A] {
    val value = t
    val ord = implicitly[Order[A]]
  }

  def OrdSeq[A: Order](as: A*): OrdSeq[A] = {
    implicit def keyMonoid[A] = new Monoid[Option[A]] {
      def append(k1: Option[A], k2: => Option[A]) = k2 orElse k1

      val zero: Option[A] = none
    }
    implicit def keyer[A] = Reducer((a: A) => some(a))
    as.foldLeft(ordSeq(FingerTree.empty[Option[A], A]))((x, y) => x insert y)
  }
}

object OrdSeq extends OrdSeqs