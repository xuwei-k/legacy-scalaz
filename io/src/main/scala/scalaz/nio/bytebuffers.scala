package scalaz.io
import scalaz._
import iteratees._
import Scalaz._
import java.nio.ByteBuffer
import collection.mutable.Buffer


/** Iteratee's that read streams of byte buffers and make sense
 * of them. 
 */
object ByteBuffers {

  // TODO(joshuasuereth): Do we make a mutable bytebuffer monoid for performance?
//
//  /** Parses an int out of a stream of ByteBuffers. */
//  def parseInt : IterV[ByteBuffer, Long] =  for {
//    buf <- bufferComplete(4)
//  } yield buf.getInt
//  /** Parses a long out of a stream of ByteBuffers. */
//  def parseFloat : IterV[ByteBuffer, Float] = for {
//    buf <- bufferComplete(4)
//  } yield buf.getFloat
//  /** Parses a long out of a stream of ByteBuffers. */
//  def parseLong : IterV[ByteBuffer, Long] = for {
//    buf <- bufferComplete(8)
//  } yield buf.getLong
//  /** Parses a long out of a stream of ByteBuffers. */
//  def parseDouble : IterV[ByteBuffer, Double] = for {
//    buf <- bufferComplete(8)
//  } yield buf.getDouble
//  /** Reads in byte buffers until they are at least the given size.
//    * This may return the raw buffer and is not guaranteed to be 'safe'.
//    */
//  def bufferComplete(size : Int) : IterV[ByteBuffer, ByteBuffer] = {
//    def step(current : ByteBuffer) : Input[ByteBuffer] => IterV[ByteBuffer, ByteBuffer] = {
//      case El(x) if current.remaining() > x.remaining() =>
//        current.put(x)
//        Cont(step(current))
//      case El(x) if current.remaining() == size =>
//        Done(x, El(x))
//      case El(x) =>
//        val tmp = new Array[Byte](current.remaining)
//        x.get(tmp)
//        current.put(tmp)
//        current.flip
//        Done(current, El(x))
//      case IterV.Empty() => Cont(step(current))
//      case EOF() => // TODO(joshuasuereth): Error?
//        Cont(step(current))
//    }
//    // TODO - Avoid creating byte buffer (array) unless needed.
//    Cont(step(ByteBuffer.allocate(size)))
//  }
}
