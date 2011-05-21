package scalaz.nio

import scalaz._
import effects.IO
import concurrent.Promise
import iteratees._
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.channels._
import java.net.{InetSocketAddress, InetAddress}
import spi.SelectorProvider
import collection.JavaConversions._
import java.nio.charset.{CodingErrorAction, CharsetDecoder}
import java.nio.charset.Charset
import Scalaz._

object streams {


  def decode[M[_],A](charset :  Charset = Charset.defaultCharset) = decodeWithEncoder[M,A](
    charset.newDecoder().onMalformedInput(
      CodingErrorAction.REPLACE).onUnmappableCharacter(
        CodingErrorAction.REPLACE))

  private def copyToBuf(in : ByteBuffer, out : ByteBuffer) : Boolean = {
    // If output is limited, compact it so we can continue filling.
    if (out.limit() == out.capacity()) {
      out.compact();
      out.limit(out.position());
      out.position(0);
    }
    val read = math.min(in.remaining, out.remaining)
    in.get(out.array, out.arrayOffset, read)
    out.position(out.position + read)
    // TOOD - update the out pointer...
    in.remaining > 0
  }
  def decodeWithEncoder[M[_], A](decoder : CharsetDecoder) = new Enumeratee[ByteBuffer,CharBuffer, M,A] {
    def apply(i : Iteratee[CharBuffer,M,A])(implicit m : Monad[M]) : Iteratee[ByteBuffer, M, Iteratee[CharBuffer,M,A]] = {
      val buf = ByteBuffer.allocate(8 * 1024) // Allocate a buffer to hold data while we decode.
      val charbuf = CharBuffer.allocate(1024);
      def step(current : Iteratee[CharBuffer,M,A]) : Input[ByteBuffer] => Iteratee[ByteBuffer, M, Iteratee[CharBuffer, M, A]] = {
        case i @ Chunk(in) =>
          copyToBuf(in, buf)
          buf.flip()
          charbuf.clear()
          val result = decoder.decode(buf, charbuf, false)
          if (result.isError) {
            iteratees.Failure(result.toString, i)
          } else {
            // Send to iteratee...
            charbuf.flip()
            val next = FlattenI(current <<: enumInput(Chunk(charbuf)))
            if (in.hasRemaining) {
              step(next)(Chunk(in))
            } else FlattenI(next.fold[Iteratee[ByteBuffer, M, Iteratee[CharBuffer, M, A]]](
              done = (_, _) => Done(next, Chunk(in)).pure,
              error = (msg, _) => iteratees.Failure(msg, i).pure,
              cont = (_) => Cont(step(current)).pure
            ))
          }
        case e @ EOF(None) =>
          val result = decoder.decode(buf, charbuf, true)
          decoder.flush(charbuf)
          if (result.isError)
            iteratees.Failure(result.toString, e)
          else {
            charbuf.flip()
            val result = FlattenI(FlattenI(current <<: enumInput(Chunk(charbuf))) <<: enumEof)
            Done(result, e)
          }
        case e @ EOF(Some(err)) => iteratees.Failure(err, e)
      }
      Cont(step(i))
    }
  }
}