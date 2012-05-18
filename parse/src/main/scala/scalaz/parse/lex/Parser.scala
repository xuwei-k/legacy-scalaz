//package scalaz
//package parse
//package lex
//
//import Result._
//
//sealed trait LexReturn[T]
//case class Return[T](t: T, s: String, cont: String => LexReturn[T]) extends LexReturn[T]
//case class Finish[T]() extends LexReturn[T]
//
//object LexReturn {
//  def return_[T](t: T, s: String, cont: String => LexReturn[T]): LexReturn[T] = new Return(t, s, cont)
//  def finish[T]: LexReturn[T] = Finish()
//}
//
//object Parser {
//  def apply[T, A](f: LexReturn[T] => Result[LexReturn[T], A]): Parser[T, A] = new Parser[T, A] {
//    def apply(ret: LexReturn[T]) = f(ret)
//  }
//
//  def runParser[T, A](pa: Parser[T, A], ret: LexReturn[T]): (Either[List[Char], A], List[Char]) = {
//    def stripLex(ret: LexReturn[T]): List[Char] = ret match {
//      case Finish() => List.empty
//      case Return(_, s, _) => s.toList
//    }
//    val (left, right) = resultToEither(pa(ret))
//    (left, stripLex(right))
//  }
//}
//
//trait Parser[T, A] {
//  def apply(ret: LexReturn[T]): Result[LexReturn[T], A]
//  def flatMap[B](f: A => Parser[T, B]): Parser[T, B] = Parser(input =>
//    apply(input).flatMap {
//      case (v, out) => apply(f(v), out)
//    }
//  )
//  def map[B](f: A => B): Parser[T, B] = Parser(input =>
//    apply(input).map { case (v, out) => (f(v), out)}
//  )
//}
//
//trait ParserInstances {
//  implicit def parserFunctor[T]: Functor[({type l[a] = Parser[T, a]})#l] = new Functor[({type l[a]=Parser[T, a]})#l] {
//    def map[A, B](fa: Parser[T, A])(f: (A) => B) = Parser((ret) => fa(ret).map(f))
//  }
//}