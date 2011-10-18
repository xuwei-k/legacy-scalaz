package scalaz

trait Plus[F[_]] {
  def plus[A](a1: F[A], a2: => F[A]): F[A]

  def deriving[G[_]](implicit n: ^**^[G, F]): Plus[G] =
    new Plus[G] {
      def plus[A](a1: G[A], a2: => G[A]) =
        n.pack(Plus.this.plus(n.unpack(a1), n.unpack(a2)))
    }

}

object Plus extends Pluss

trait Pluss {
  implicit def OptionPlus: Plus[Option] = new Plus[Option] {
    def plus[A](a1: Option[A], a2: => Option[A]) =
      a1 orElse a2
  }

  implicit def ListPlus: Plus[List] = new Plus[List] {
    def plus[A](a1: List[A], a2: => List[A]) =
      a1 ::: a2
  }

  implicit def StreamPlus: Plus[Stream] = new Plus[Stream] {
    def plus[A](a1: Stream[A], a2: => Stream[A]) =
      a1 #::: a2
  }
  
  implicit def StateTPlus[A, F[_]](implicit p: Plus[F]): Plus[({type λ[α] = StateT[A, F, α]})#λ] = new Plus[({type λ[α] = StateT[A, F, α]})#λ] {
    def plus[B](a1: StateT[A, F, B], a2: => StateT[A, F, B]) = 
      StateT(s => p.plus(a1 runT s, a2 runT s))
  }
  
  implicit def KleisliPlus[A, F[_]](implicit p: Plus[F]): Plus[({type λ[α] = Kleisli[A, F, α]})#λ] = new Plus[({type λ[α] = Kleisli[A, F, α]})#λ] {
    def plus[B](a1: Kleisli[A, F, B], a2: => Kleisli[A, F, B]) =
      Kleisli(s => p.plus(a1 run s, a2 run s))
  }
  
  implicit def OptionTPlus[F[_]](implicit m: Monad[F]): Plus[({type λ[α] = OptionT[F, α]})#λ] = new Plus[({type λ[α] = OptionT[F, α]})#λ] {
    def plus[B](a1: OptionT[F, B], a2: => OptionT[F, B]) = {
      OptionT((m.bd((o: Option[B]) => o.map(_ => m.point(o)) getOrElse(a2.runT)))(a1.runT))
    }
  }
}
