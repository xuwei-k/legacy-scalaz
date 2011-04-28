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
  def enumByteChannel(channel : IO[JByteChannel], buf : ByteBuffer) = new Enumerator[ByteBuffer, IO] {
    private def readBuf(channel : IO[JByteChannel]) : IO[(JByteChannel,Input[ByteBuffer])] = channel map { c =>
      buf.rewind()
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
    def apply[A](i : Iteratee[ByteBuffer,IO,A])(implicit m : Monad[IO]) : IO[Iteratee[ByteBuffer,IO,A]] = {
      def drive(i : Iteratee[ByteBuffer, IO, A], state : IO[JByteChannel]) : IO[Iteratee[ByteBuffer, IO,A]] =
        state flatMap { channel =>
          if(!channel.isOpen) m.pure(i) else {
              i.fold[Iteratee[ByteBuffer,IO,A]](
                done = (_,_) => m.pure(i),
                cont = (k) => readBuf(state).flatMap { case (channel, input) =>
                  drive(k(input), m.pure(channel))   // TODO - This isn't 'scala' tail recursion, maybe we should trampoline?
                },
                error = (_) => m.pure(i)
              )
          }
        }
      drive(i, channel)
    }
  }
  /** An iteratee that will write to a byte channel */
  def writeToChannel[A](channel: IO[JByteChannel])(implicit m: Monad[IO]): Iteratee[ByteBuffer, IO, Unit] = {
    def step[A](channel: IO[JByteChannel]): Input[ByteBuffer] => Iteratee[ByteBuffer, IO, Unit] = {
      case EOF(None) =>
        FlattenI(channel.map(_.close()).map(Done(_, EOF[ByteBuffer](None))))
      case Chunk(buf) =>
        FlattenI(channel map { c =>
          c.write(buf)
          Cont(i => step(m.pure(c))(i))
        })
      case EOF(Some(err)) => iteratees.Failure(err)
    }
    Cont(i => step(channel)(i))
  }
}
