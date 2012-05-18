//package scalaz
//package parse
//package state
//
//import base._
//import Parser._
//
//object ParserTest {
//  val p3 = for {
//    x <- next[Int, Char]
//    _ <- stUpdate[Int, Char](_ + 1)
//    y <- next[Int, Char]
//    _ <- stUpdate[Int, Char](_ + 1)
//    z <- next[Int, Char]
//    _ <- stUpdate[Int, Char](_ + 1)
//  } yield (x :: y :: z :: Nil).mkString
//
//  val p2 = for {
//    x <- next[Int, Char]
//    y <- next[Int, Char]
//  } yield (x :: y :: Nil).mkString
//
//  type P[A] = Parser[Int, Char, A]
//  val p3or2 = oneOf[String, P](List(p3, p2))
//  val m = manyFinally[String, String, P](p2, p3)
//
//  def run(s: String) = runParser(p3or2, 0, s.toList)
//}
