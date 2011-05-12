package scalaz.example.iteratees

object ExampleIteratee {
  def main(args: Array[String]) = run

  import scalaz._
  import Scalaz._
  import scalaz.iteratees._

  def streamEnumerator[I](s : Stream[I]) = new Enumerator[I, Identity] {
    def apply[A](i : Iteratee[I,Identity,A])(implicit m : Monad[Identity]) : Identity[Iteratee[I,Identity,A]] = {
      def drive(s : Stream[I], i : Iteratee[I,Identity,A]) : Iteratee[I,Identity, A] = s match {
        case Stream() => i
        case x #:: xs => i.fold(done = (_,_) => i, error = (_,_) => i, cont = k => drive(xs, k(Chunk(x))))
      }
      drive(s, i)
    }
  }

  def run {
    streamEnumerator(Stream(10,20,30))(readLength(ignore => 1)).run assert_=== 3L
    //head(Stream(1, 2, 3)).run assert_=== Some(1)
    //length(Stream(10, 20, 30)).run assert_=== 3
    //peek(Stream(1, 2, 3)).run assert_=== Some(1)
    //head(Stream[Int]()).run assert_=== none[Int]
    
    // As a monad
    //val m1 = head[Int] >>= ((b:Option[Int]) => head[Int] map (b2 => (b <|*|> b2)))
    //m1(Stream(1,2,3)).run assert_=== Some(1 -> 2)
    
    
  }
}
