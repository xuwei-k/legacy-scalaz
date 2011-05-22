package scalaz.nio
import scalaz._
import scalaz.effects.IO
import scalaz.iteratees._
import java.nio.ByteBuffer
import java.nio.channels.{ByteChannel=>JByteChannel}
import Scalaz._
import iteratees._

/** This object holds helper methods to use Iteratee I/O against 
 * java.nio.Channels.
 */
object Channels {

  /**
   * Construct an enumerator of nio ByteChannel's that uses the given buffer.
   * @param channel  The NIO channel to read from.
   * @param buf The buffer to use when reading from the nio channel.   This will be recycled.
   */
  def enumByteChannel[M[_]](channel : JByteChannel, buf : ByteBuffer = ByteBuffer.allocate(8192)) = new Enumerator[ByteBuffer, M] {
    def apply[A](i : Iteratee[ByteBuffer,M,A])(implicit m : Monad[M]) : M[Iteratee[ByteBuffer,M,A]] = {
      // Helper to read from the channel into the ByteBuffer.
      def readBuf(channel : M[JByteChannel]) : M[(JByteChannel,Input[ByteBuffer])] = channel map { c =>
        buf.rewind()
        if (buf.limit() == buf.capacity()) {
          buf.compact();
          buf.limit(buf.position());
          buf.position(0);
        }
        try {
          if(c.read(buf) >= 0) {
            buf.flip()
            (c, Chunk(buf))
          } else {
            // Close channel?
            c.close()
            (c, EOF(None))
          }
        } catch {
          case x : java.io.IOException => (c, EOF(Some(x.getMessage)))
        }
      }
      /// Helper to drive ByteBuffers through the Iteratee.
      def drive(i : Iteratee[ByteBuffer, M, A], state : M[JByteChannel]) : M[Iteratee[ByteBuffer, M,A]] =
        state flatMap { channel =>
          if(!channel.isOpen) m.pure(i) else {
              i.fold[Iteratee[ByteBuffer,M,A]](
                done = (_,_) => m.pure(i),
                cont = (k) => readBuf(state).flatMap { case (channel, input) =>
                  drive(k(input), m.pure(channel))   // TODO - This isn't 'scala' tail recursion, maybe we should trampoline?
                },
                error = (_, _) => m.pure(i)
              )
          }
        }
      drive(i, channel.pure)
    }
  }
  /** An iteratee that will write to a byte channel */
  def writeToChannel[M[_]](channel: M[JByteChannel])(implicit m: Monad[M]): Iteratee[ByteBuffer, M, Unit] = {
    def step[A](channel: M[JByteChannel]): Input[ByteBuffer] => Iteratee[ByteBuffer, M, Unit] = {
      case EOF(None) =>
        FlattenI(channel.map(_.close()).map(Done(_, EOF[ByteBuffer](None))))
      case Chunk(buf) =>
        FlattenI(channel map { c =>
          c.write(buf)
          Cont(x => step(m.pure(c))(x))
        })
      case EOF(e@Some(err)) => iteratees.Failure(err, EOF(e))
    }
    Cont(i => step(channel)(i))
  }

  // TODO - enumeratee to convert from ByteBuffer to Char stream...
  // TODO - enumeratee to convert ByteBuffer stream to 'Serialized java object' stream?
}
