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
  // TODO - Temprrary for testing:
  def fileInputChannel(file : java.io.File)(implicit m : Monad[IO]) : IO[JByteChannel] =
    m.pure(file).map(new java.io.FileInputStream(_)).bracket(
        i => {i.close(); m.pure(())}
      )(i => m.pure(i.getChannel()))


  def enumByteChannel[A](channel : IO[JByteChannel], buf : ByteBuffer) = new Enumerator[ByteBuffer, IO] {
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
//  /** Abstract representation of an nio Channel.
//    * TODO(jsuereth): Laziness and Promises for async?
//    */
//  sealed trait InputChannel[A] {
//    def readNext : Input[A]
//    def isOpen : Boolean
//    def close() : Unit
//  }
//  sealed trait OutputChannel[A] {
//    def write(value :A) : Unit
//    def isOpen : Boolean
//    def close() : Unit
//  }
//  def byteBufferOutputChannel(native : JByteChannel) : OutputChannel[ByteBuffer] = new OutputChannel[ByteBuffer] {
//    def isOpen = native.isOpen
//    def close() = native.close()
//    def write(value : ByteBuffer) : Unit =
//      native.write(value)
//  }
//  /** Wraps a ByteChannel with a sized ByteBuffer that is used to read values and feed
//   * them to the Iteratee IO
//   */
//  def byteBufferInputChannel(native : JByteChannel, buf : ByteBuffer) : InputChannel[ByteBuffer] = new InputChannel[ByteBuffer] {
//     def isOpen = native.isOpen
//     def close() = native.close()
//     def readNext = {
//       buf.rewind()
//       val size = native.read(buf)
//       if(size >=0) {
//         buf.flip()
//         El(buf)
//       } else EOF[ByteBuffer]
//     }
//  }
//
//  /** Enumerator that will drive data from a given input through an Iteratee. */
//  implicit object ChannelEnumerator extends Enumerator[InputChannel] {
//      def apply[E, A](channel: InputChannel[E], i: IterV[E, A]): IterV[E, A] = {
//        @annotation.tailrec
//        def step(io : IterV[E, A]) : IterV[E,A] =
//          io match {
//            case i if !channel.isOpen => i
//            case i @ Done(_,_) =>
//               // TODO(jsuereth): make this lazy *and* perhaps wrap entire resource in some other magikz.
//               channel.close()
//               i
//            case Cont(k) =>
//               step(k(channel.readNext))
//          }
//        step(i)
//      }
//  }
//
//  /** Iteratee that will write to a ByteChannel. */
//  // TODO(jsuereth): Laziness + Safety (IO monad)?
//  def writeToChannel[A](channel : OutputChannel[ByteBuffer]) : IterV[ByteBuffer, Unit] = {
//    def step[A](channel : OutputChannel[ByteBuffer]) : Input[ByteBuffer] => IterV[ByteBuffer, Unit] = {
//      case EOF() =>
//        channel.close()
//        Done((), EOF[ByteBuffer])
//      case El(buf) =>
//        channel.write(buf)
//        Cont(step(channel))
//      case IterV.Empty() =>
//        Cont(step(channel))
//    }
//    Cont(step(channel))
//  }
}
