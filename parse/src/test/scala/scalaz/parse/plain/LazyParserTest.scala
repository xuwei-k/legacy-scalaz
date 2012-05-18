//package scalaz
//package parse
//package plain
//
//import base._
//import LazyParser._
//
//object LazyParserTest {
//  val p3 = for {
//    x <- next[Char]
//    y <- next[Char]
//    z <- next[Char]
//  } yield (x :: y :: z :: Nil).mkString
//
//  val p2 = for {
//    x <- next[Char]
//    y <- next[Char]
//  } yield (x :: y :: Nil).mkString
//
//  type P[A] = LazyParser[Char, A]
//  val p3or2: LazyParser[Char, String] = oneOf[String, P](List(p3, p2))
//}
