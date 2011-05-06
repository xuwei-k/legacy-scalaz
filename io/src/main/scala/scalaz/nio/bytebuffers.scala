package scalaz.nio
import scalaz._
import iteratees._
import Scalaz._
import collection.mutable.Buffer
import java.nio.{CharBuffer, ByteBuffer}
import java.nio.charset.{CoderResult, Charset}


/** Iteratee's that read streams of byte buffers and make sense
 * of them.
 */
object CharStreams {

  def lamePrinter[M[_] : Monad] : Iteratee[CharBuffer, M, Unit] = {
    def step(in : Input[CharBuffer]) : Iteratee[CharBuffer,M,Unit] = in match {
      case Chunk(buf) =>
       Console.print(buf)
       Cont(step)
      case x =>  Done((), x)
    }
    Cont(step)
  }

  /**
   * Constructs an enumeratee that drives Character streams from ByteBuffer streams.
   */
  def charStreamFromByteStream[M[_] : Monad](charset: Charset = Charset.defaultCharset,
                                   bbuffer: ByteBuffer = ByteBuffer.allocate(8192),
                                   cbuffer: CharBuffer = CharBuffer.allocate(4096)) = new Enumeratee[ByteBuffer, CharBuffer, M] {
    private type Result[A] = Iteratee[CharBuffer,M,A]
    lazy val decoder = charset.newDecoder
    /**
     * Lame mutable mechanism to flip buffers and read from the 'stream' buffer into our persistent buffer
     * for character decoding.
     */
    private def fillWithoutOverflow(cur: ByteBuffer, next: ByteBuffer) {
      cur.flip()
      if (cur.remaining < next.remaining) {
        val bytes = new Array[Byte](cur.remaining)
        next.get(bytes)
        cur.put(bytes)
      } else cur.put(next)
      cur.flip()
    }

    /**Attempts to read all characters possible out of a byte stream.   Remaining bytes are left in the current
     * ByteBuffer and passed into the next step function.
     * Note: When this is done driving the current input, it calls the step function with the left-over state.
     */
    @annotation.tailrec
    private def readAndDrive[A](out : Result[A], current : ByteBuffer, next : Option[ByteBuffer]): Iteratee[ByteBuffer,M, Result[A]] = {
      next.foreach(fillWithoutOverflow(current,_))
      cbuffer.rewind()
      decoder.decode(current, cbuffer, next.isEmpty) match {
        case CoderResult.OVERFLOW =>
          cbuffer.flip()
          readAndDrive(FlattenI(enumInput(Chunk(cbuffer))(out)), current, next)
        case CoderResult.UNDERFLOW =>
          cbuffer.flip()
          val nextI = FlattenI(enumInput(Chunk(cbuffer))(out))
          if (next.map(_.remaining > 0).getOrElse(false)) readAndDrive(nextI, current, next)
          else Cont(step(nextI, current))
        case err =>  iteratees.Failure(err.toString)
      }
    }
    /** Represents a processing step in converting a byte stream to a character stream */
    private def step[A](cur : Result[A], buf : ByteBuffer)(in : Input[ByteBuffer]) : Iteratee[ByteBuffer, M, Result[A]] =
      in match {
        case Chunk(bytebuf) =>
          readAndDrive(cur, buf, Some(bytebuf))
        case EOF(Some(msg)) => iteratees.Failure(msg)
        case EOF(None) =>
          if(buf.remaining > 0) {
            FlattenI(enumEof(readAndDrive(cur,buf,None)))
          } else Done(FlattenI(enumEof(cur)), EOF(None))
      }
    def apply[A](i : Iteratee[CharBuffer,M,A]) : Iteratee[ByteBuffer, M, Iteratee[CharBuffer,M,A]] =
      Cont(step(i, bbuffer))
  }

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
