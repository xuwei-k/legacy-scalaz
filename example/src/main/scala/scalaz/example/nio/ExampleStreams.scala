package scalaz.example.nio

import scalaz._
import concurrent.Promise
import effects.IO
import iteratees._
import nio.streams._
import nio.Channels._
import Scalaz._
import java.nio.channels.SocketChannel
import java.nio.channels.ByteChannel
import java.nio.{CharBuffer, ByteBuffer}

object ExampleStreams {

  // We use a strict Id monad here because Identity cause strange and hard-to-decipher bugs
  type Id[A] = A

  implicit object idmonad extends Monad[Id] {
    def pure[A](a : => A) : A = a
    def bind[A,B](a : Id[A], f : A => Id[B]) : Id[B] = f(a)
  }



  def run {
    val printer : Iteratee[String, Id, Unit] = {
      def step(lineno : Long): (Input[String] => Iteratee[String, Id, Unit]) = {
        case Chunk(line) =>
          Console.println("line " + lineno + ": " + line)
          Cont(step(lineno+1))
        case e @ EOF(_) => Done((), e)
      }
      Cont(step(1))
    }

    val aggregator : Iteratee[String, Id, Vector[String]] = {
      def step(vector : Vector[String]): (Input[String] => Iteratee[String, Id, Vector[String]]) = {
        case Chunk(line) =>
          Cont(step(vector :+ line))
        case e @ EOF(_) => Done(vector, e)
      }
      Cont(step(Vector()))
    }
    val processor = (printer zip aggregator) map {
      case (_, lines) => lines
    }

    val cora = new java.io.FileInputStream("test.txt")
    val e = enumByteChannel[Id](cora.getChannel.asInstanceOf[ByteChannel])


    /*val charbufferPrinter : Iteratee[CharBuffer,Identity,Vector[String]] =
      joinI(lines(processor))
    val bytebufferPrinter : Iteratee[ByteBuffer,Identity,Vector[String]] =
      joinI(decode()(charbufferPrinter))*/

    val charbufferPrinter : Iteratee[CharBuffer,Id,Iteratee[String, Id, Vector[String]]] =
      lines(processor)

    val bytebufferPrinter : Iteratee[ByteBuffer,Id,Iteratee[CharBuffer,Id,Iteratee[String, Id, Vector[String]]]] =
      decode()(charbufferPrinter)
    val result = bytebufferPrinter <<: e
    for ( (line, idx) <- result.run.run.run.zipWithIndex) {
      Console.println("line " + idx + ": " + line)
    }
  }
}