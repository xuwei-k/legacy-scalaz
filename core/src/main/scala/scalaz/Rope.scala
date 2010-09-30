package scalaz

import scalaz.{ImmutableArray => IA}
import collection.generic.CanBuildFrom
import collection.mutable.Builder
import collection.IndexedSeqLike

sealed class Rope[A: ClassManifest](val value: FingerTreeIntPlus[ImmutableArray[A]])
        extends NewType[FingerTreeIntPlus[ImmutableArray[A]]] {
  import Rope._
  import FingerTree._

  implicit def sizer = Reducer((arr: ImmutableArray[A]) => arr.length)

  def length = value.measure

  def apply(i: Int): A = {
    val split = value.split(_ > i)
    split._2.viewl.headOption.getOrElse(error("Index " + i + " > " + value.measure))(i - split._1.value.measure)
  }

  def ++(xs: Rope[A]) = rope(value <++> xs.value)

  def ::+(chunk: ImmutableArray[A]) =
    if (chunk.isEmpty)
      this
    else
      rope(
        value.viewr.fold(
          single(chunk),
          (_, last) =>
            if (last.length + chunk.length <= baseChunkLength)
              value :-| (last ++ chunk)
            else
              value :+ chunk
          )
        )

  def +::(chunk: ImmutableArray[A]) =
    if (chunk.isEmpty)
      this
    else
      rope(
        value.viewl.fold(
          single(chunk),
          (head, _) =>
            if (chunk.length + head.length <= baseChunkLength)
              (chunk ++ head) |-: value
            else
              chunk +: value
          )
        )

  def :+(x: A) = this ::+ IA.fromArray(Array(x))

  def +:(x: A) = IA.fromArray(Array(x)) +:: this

  def tail = rope(value.tail)

  def init = rope(value.init)
  //      def map[B](f: A => B) = rope(value map f)
  //      def flatMap[B](f: A => Rope[B]) =
  //        rope(value.foldl(empty[Int, B])((ys, x) => ys <++> f(x).value))

  // override def foreach[U](f: A => U): Unit = value.foreach(_.foreach(f))

  def iterator: Iterator[A] = value.iterator.flatMap(_.iterator)

  def reverseIterator: Iterator[A] = value.reverseIterator.flatMap(_.reverseIterator)

  // TODO override def reverse

  def chunks = value.toStream

  // protected[this] override def newBuilder: Builder[A, Rope[A]] = new RopeBuilder[A]
}

object Rope extends Ropes {
  import FingerTree._
  
  private[scalaz] val baseChunkLength = 16

  implicit def sizer[A]: Reducer[ImmutableArray[A], Int] = Reducer(_.length)

  def empty[A: ClassManifest] = rope(FingerTree.empty[Int, ImmutableArray[A]])

  def fromArray[A: ClassManifest](a: Array[A]): Rope[A] =
    if (a.isEmpty) empty[A] else rope(single(IA.fromArray(a)))

  def fromString(str: String): Rope[Char] =
    if (str.isEmpty) empty[Char] else rope(single(IA.fromString(str)))

  def fromChunks[A: ClassManifest](chunks: Seq[ImmutableArray[A]]): Rope[A] =
    rope(chunks.foldLeft(FingerTree.empty[Int, ImmutableArray[A]])((tree, chunk) => if (!chunk.isEmpty) tree :+ chunk else tree))
  //      def apply[A](as: A*) = fromSeq(as)
  //      def fromSeq[A](as: Seq[A]) = rope(as.foldLeft(empty[Int, A](Reducer(a => 1)))((x, y) => x :+ y))

  def newBuilder[A: ClassManifest]: Builder[A, Rope[A]] = new RopeBuilder[A]

  implicit def canBuildFrom[T: ClassManifest]: CanBuildFrom[Rope[_], T, Rope[T]] =
    new CanBuildFrom[Rope[_], T, Rope[T]] {
      def apply(from: Rope[_]): Builder[T, Rope[T]] = newBuilder[T]

      def apply: Builder[T, Rope[T]] = newBuilder[T]
    }
}

sealed class WrappedRope[A: ClassManifest](val value: Rope[A])
        extends NewType[Rope[A]] with IndexedSeq[A] with IndexedSeqLike[A, WrappedRope[A]] {
  import Rope._

  import FingerTree._

  def apply(i: Int): A = value(i)

  def ++(xs: WrappedRope[A]) = wrapRope(value ++ xs.value)

  // override def :+(x: A) = wrapRope(value :+ x)
  // override def +:(x: A) = wrapRope(x +: value)
  override def tail = rope(value.tail)

  override def init = rope(value.init)
  //      def map[B](f: A => B) = rope(value map f)
  //      def flatMap[B](f: A => Rope[B]) =
  //        rope(value.foldl(empty[Int, B])((ys, x) => ys <++> f(x).value))

  // override def foreach[U](f: A => U): Unit = value.foreach(_.foreach(f))

  override def iterator: Iterator[A] = {
    value.value.iterator.flatMap(_.iterator)
  }

  override def reverseIterator: Iterator[A] = value.value.reverseIterator.flatMap(_.reverseIterator)

  // TODO override def reverse

  override def toStream = value.chunks.flatten

  override def length = value.length

  protected[this] override def newBuilder = new RopeBuilder[A].mapResult(wrapRope(_))
}

trait Ropes {
  def rope[A: ClassManifest](v: FingerTreeIntPlus[ImmutableArray[A]]) = new Rope[A](v)


  implicit def wrapRope[A: ClassManifest](rope: Rope[A]): WrappedRope[A] = new WrappedRope(rope)

  implicit def unwrapRope[A: ClassManifest](wrappedRope: WrappedRope[A]): Rope[A] = wrappedRope.value

  final class RopeBuilder[A: ClassManifest] extends Builder[A, Rope[A]] {
    import Rope._
    private var startRope: Rope[A] = Rope.empty[A]
    private var tailBuilder: Builder[A, ImmutableArray[A]] = IA.newBuilder[A]
    private var tailLength = 0

    def clear {
      startRope = Rope.empty[A]
      tailBuilder = IA.newBuilder[A]
      tailLength = 0
    }

    def +=(elem: A) = {
      if (tailLength < baseChunkLength) {
        tailBuilder += elem
        tailLength += 1
      }
      else {
        cleanTail
        tailBuilder += elem
        tailLength = 1
      }
      this
    }

    def result = startRope ::+ tailBuilder.result

    override def sizeHint(size: Int) {
      tailBuilder.sizeHint(math.min(size - startRope.length, baseChunkLength))
    }

    // TODO fix and reinstate
    //      import collection.mutable.ArrayLike
    //      override def ++=(xs: TraversableOnce[A]) = {
    //        xs match {
    //          case xs: Rope[A] => {
    //            cleanTail
    //            startRope ++= xs
    //          }
    //          case xs: ImmutableArray[A] => {
    //            cleanTail
    //            startRope ::+= xs
    //          }
    //          case xs: ArrayLike[A, _] => {
    //            cleanTail
    //            tailBuilder ++= xs
    //          }
    //          case _ =>  super.++=(xs)
    //        }
    //        this
    //      }
    //    }

    private def cleanTail {
      startRope ::+= tailBuilder.result
      tailBuilder.clear
    }
  }

  sealed class RopeCharW(val value: Rope[Char]) extends PimpedType[Rope[Char]] {
    def asString = {
      val stringBuilder = new StringBuilder(value.length)
      appendTo(stringBuilder)
      stringBuilder.toString
    }

    def appendTo(stringBuilder: StringBuilder) {
      value.chunks.foreach(ia => stringBuilder.append(ia.asString))
    }
  }

  implicit def wrapRopeChar(rope: Rope[Char]): RopeCharW = new RopeCharW(rope)
}
