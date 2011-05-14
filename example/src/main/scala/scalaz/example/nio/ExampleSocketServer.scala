package scalaz.example.nio

import scalaz._
import concurrent.Promise
import effects.IO
import nio.sockets._
import nio.Channels._
import Scalaz._
import iteratees._
import java.nio.channels.SocketChannel
import java.nio.channels.ByteChannel
import java.nio.ByteBuffer

object ExampleSocketServer {
  def run(implicit s: concurrent.Strategy) {
    // TODO - Massive cleanup of Iteratee API and lame map/flatMap usage.
    // Also, find a way to have the server close the connection so we can start up a client
    // on another thread and see what happens.
    val server = serverSocketChannel(8080)
    val firstConnection : IO[Promise[SocketChannel]] =
       server.map(s => FlattenI(s(head[Promise, SocketChannel]))).map(_.run.map(_.get))
    val echo : IO[Iteratee[ByteBuffer, Promise, Unit]] = firstConnection flatMap { futureSocket =>
      val consumer = writeToChannel[Promise](futureSocket.map(_.asInstanceOf[ByteChannel]))
      val reader = futureSocket map (i => readSocketChannel(i))
      reader.get map (e => FlattenI(e(consumer)))
    }
    val runner : IO[Unit] = echo.map(_.run)
    runner.unsafePerformIO
  }
}