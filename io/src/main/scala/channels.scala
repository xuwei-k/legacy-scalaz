package scalaz.io
import scalaz._
import java.nio.ByteBuffer
import java.nio.channels.{ByteChannel=>JByteChannel}
import Scalaz._
import IterV._

/** This object holds helper methods to use Iteratee I/O against 
 * java.nio.Channels.
 */
object Channels {
  /** Abstract representation of an nio Channel.
    * TODO(jsuereth): Laziness and Promises for async?
    */
  sealed trait InputChannel[A] {
    def readNext : Input[A]
    def isOpen : Boolean
    def close() : Unit
  }
  sealed trait OutputChannel[A] {
    def write(value :A) : Unit
    def isOpen : Boolean
    def close() : Unit
  }
  def byteBufferOutputChannel(native : JByteChannel) : OutputChannel[ByteBuffer] = new OutputChannel[ByteBuffer] {
    def isOpen = native.isOpen
    def close() = native.close()
    def write(value : ByteBuffer) : Unit =
      native.write(value)
  }
  /** Wraps a ByteChannel with a sized ByteBuffer that is used to read values and feed
   * them to the Iteratee IO 
   */
  def byteBufferInputChannel(native : JByteChannel, buf : ByteBuffer) : InputChannel[ByteBuffer] = new InputChannel[ByteBuffer] {
     def isOpen = native.isOpen
     def close() = native.close()
     def readNext = {
       buf.rewind()
       val size = native.read(buf)
       if(size >=0) {
         buf.flip()
         El(buf)
       } else EOF[ByteBuffer]
     }
  }
  /** Enumerator that will drive data from a given input through an Iteratee. */
  implicit object ChannelEnumerator extends Enumerator[InputChannel] {
      def apply[E, A](channel: InputChannel[E], i: IterV[E, A]): IterV[E, A] = {
        @annotation.tailrec
        def step(io : IterV[E, A]) : IterV[E,A] =
          io match {
            case i if !channel.isOpen => i
            case i @ Done(_,_) => 
               // TODO(jsuereth): make this lazy *and* perhaps wrap entire resource in some other magikz.
               channel.close()
               i
            case Cont(k) =>
               step(k(channel.readNext))
          }
        step(i)
      }
  }

  /** Iteratee that will write to a ByteChannel. */
  // TODO(jsuereth): Laziness + Safety (IO monad)?
  def writeToChannel[A](channel : OutputChannel[ByteBuffer]) : IterV[ByteBuffer, Unit] = {
    def step[A](channel : OutputChannel[ByteBuffer]) : Input[ByteBuffer] => IterV[ByteBuffer, Unit] = {
      case EOF() =>
        channel.close()
        Done((), EOF[ByteBuffer])
      case El(buf) =>
        channel.write(buf)
        Cont(step(channel))
      case IterV.Empty() =>
        Cont(step(channel))
    }
    Cont(step(channel))
  }
}
