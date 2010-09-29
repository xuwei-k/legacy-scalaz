package scalaz.example

import scalaz._
import scalaz.concurrent.Promise

//object ExampleNewType {
//  def main(args: Array[String]) = run
//
//  import Scalaz._
//
//  def run {
//    trait OptionT[F[_], G[_]] {
//
//    }
//
//    object OptionT {
//      trait OptionM[M[_]] {
//        type Apply[T] = M[Option[T]]
//      }
//      def apply[M[_]](implicit m: Monad[M]): Monad[OptionM[M]#Apply] = new Monad[OptionM[M]#Apply] {
//        def bind[A, B](a: M[Option[A]], f: (A) => M[Option[B]]) = a ∗ (oa map f)
//
//        def pure[A](a: A) = m.pure(a.some)
//      }
//    }
//
//    case class PromiseValidationNEL[A](value: Promise[ValidationNEL[String, A]]) extends NewType[Promise[ValidationNEL[String, A]]]
//
//    object PromiseValidationNEL {
//      implicit val ToPromiseValidationNEL = PromiseValidationNEL.apply _
//
//      implicit val FunctorPromiseValidationNEL: Functor[PromiseValidationNEL] = new Functor[PromiseValidationNEL] {
//        def fmap[A, B](r: PromiseValidationNEL[A], f: (A) => B) = r.value ∘∘ f
//      }
//      implicit val PurePromiseValidationNEL: Pure[PromiseValidationNEL] = new Pure[PromiseValidationNEL] {
//        def pure[A](a: A) = a.pure[PartialApply1Of2[ValidationNEL, String]#Apply, A].pure[Promise]
//      }
//      implicit val BindPromiseValidationNEL: Bind[PromiseValidationNEL] = new Bind[PromiseValidationNEL] {
//        def bind[A, B](a: PromiseValidationNEL[A], f: (A) => PromiseValidationNEL[B]) = {
//          val x: Promise[ValidationNEL[String, PromiseValidationNEL[B]]] = a.value ∘∘ f
//          x ∘
//
//        }
//      }
//    }
//  }
//}
