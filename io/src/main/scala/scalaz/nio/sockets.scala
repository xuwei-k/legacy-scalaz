package scalaz.nio

import scalaz._
import effects.IO
import concurrent.Promise
import iteratees._
import java.nio.ByteBuffer
import java.nio.channels._
import Scalaz._
import java.net.{InetSocketAddress, InetAddress}
import spi.SelectorProvider
import collection.JavaConversions._


object sockets {

  /**Creates a socket server where connections are farmed out inside promises.   Note:
   * The select calls are defered using the Promise monad and the implicitly supplied
   * concurrent strategy.
   *
   * TODO(joshuasuereth): Allow accepting connections to happen on multiple threads, or perhaps execute selects
   * using the strategy passed in.
   *
   * @param port  The port to bind the server socket to.
   * @param s  The threading strategy to use when farming out accepted connections for processing.
   */
  def serverSocketChannel(port : Int)(implicit s : concurrent.Strategy) : IO[Enumerator[SocketChannel, Promise]] =
    // This method captures 'unsafe' IO bits that we can embedd in our IO controller.
    IO.ioPure.pure(new Enumerator[SocketChannel, Promise] {
        val selector = Selector.open() // TODO - use SelectorProvider?
        val selectableChannel: ServerSocketChannel = ServerSocketChannel.open()
        selectableChannel.configureBlocking(false)
        val socketAddr = new InetSocketAddress(port)
        selectableChannel.socket.bind(socketAddr)
        selectableChannel.register(selector, SelectionKey.OP_ACCEPT)
        def apply[A](i : Iteratee[SocketChannel,Promise,A])(implicit m : Monad[Promise]) : Promise[Iteratee[SocketChannel,Promise,A]] = {
          def nextInput : Input[SocketChannel] = try {
            // TODO - Use a more FP approach here
            // TODO - Don't block forever!
            if (selector.select > 0) {
              val readyKeys = selector.selectedKeys()
              val itr = readyKeys.iterator()
              while (itr.hasNext()) {
                val key = itr.next().asInstanceOf[SelectionKey]
                itr.remove();
                if (key.isAcceptable() ) {
                  val sschanel = key.channel().asInstanceOf[ServerSocketChannel]
                  val channel = sschanel.accept();
                  return Chunk(channel)
                }
              }
            }
            // If we didn't get a result yet, keep trying
            nextInput
          } catch {
            case ex: java.io.IOException =>
              EOF(Some(ex.getMessage))
          }
          i.fold[Iteratee[SocketChannel,Promise,A]](
            done = (_,_) => Promise(i),
            cont = k => Promise(k(nextInput)).flatMap(apply),
            error = (_,_) => Promise(i)
          )
        }
      })
}


/**
 * A lame implmenetation of a non-blocking server that uses a single thread to select on events and drive an iteratee.
 * @param handler
 *      A function that takes the *output* iteratee (consumer) for a connection and returns the *input* iteratee (consumer)
 *      for the connection.   A simple echo server would just pass the identity function for the handler.
 */
class NonBlockingServer(port : Int = 4001, handler: Iteratee[ByteBuffer, Identity, Unit] => Iteratee[ByteBuffer, Identity, Unit]) {

  // Creates an iteratee for a SocketChannel that delays all writes to the selector.
  private def makeWriterIterateeFor(selector: Selector, channel: SocketChannel) : Iteratee[ByteBuffer, Identity, Unit] = {
    def step(input : Input[ByteBuffer]) : Iteratee[ByteBuffer,Identity,Unit] = input match {
      case Chunk(buf) if buf.hasRemaining =>
        // TODO(jsuereth): Defer or store in IO monad?
        channel.write(buf)
        Cont(step)
      case Chunk(_) =>
        Cont(step)
      case EOF(_) =>
        channel.close()
        Done((), input)
    }
    Cont(step)
  }

  private def handleRead(selector : Selector, channel : SocketChannel, processor : Iteratee[ByteBuffer,Identity,Unit]) : Unit = {
    // TODO - re-use byte buffers? Configure byte buffers?
    val buf = ByteBuffer.allocate(1024*8)
    val input : Input[ByteBuffer] = try {
      if (channel.read(buf) >=0) {
        buf.flip()
        Chunk(buf)
      } else EOF(None)
    } catch {
      case ex : java.io.IOException =>
        EOF(Some(ex.getMessage))
    }
    // If we were using the IO monad, we'd need to run here to drive the iteratee
    // as we go...   Let's think on this.
    val next = FlattenI(enumInput(input)(processor))
    // TODO - Peak at results to figure out if we should be done!
    channel.register(selector, SelectionKey.OP_READ, () => handleRead(selector, channel, next))
  }

  /**
   * Handles all I/O events on the current thread.
   */
  def acceptConnections() {
    val selector = Selector.open() // TODO - use SelectorProvider?
    val selectableChannel: ServerSocketChannel = ServerSocketChannel.open()
    selectableChannel.configureBlocking(false)
    val socketAddr = new InetSocketAddress(port)
    selectableChannel.socket.bind(socketAddr)
    selectableChannel.register(selector, SelectionKey.OP_ACCEPT)
    while (selector.select() > 0) {
      val readyKeys = selector.selectedKeys()
      val i = readyKeys.iterator();
      while (i.hasNext()) {
        val key = i.next().asInstanceOf[SelectionKey]
        i.remove();
        if (key.isAcceptable() ) {
          val sschanel = key.channel().asInstanceOf[ServerSocketChannel]
          val channel = sschanel.accept();
          channel.configureBlocking( false );
          val processor = handler(makeWriterIterateeFor(selector, channel))
          channel.register(selector, SelectionKey.OP_READ, () => handleRead(selector, channel, processor))
        } else {
          // TODO - Move these onto a thread pool or some other location.
          val handler = key.attachment.asInstanceOf[Function0[Unit]]
          key.interestOps(0)
          handler.apply()
        }
      }
    }
  }
}