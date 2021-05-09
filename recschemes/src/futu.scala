package recschemes

object futu {
  sealed trait CoAttr[F[_], A]
  final case class Automatic[F[_], A](a: A) extends CoAttr[F, A]
  final case class Manual[F[_], A](fa: F[CoAttr[F, A]]) extends CoAttr[F, A]

  def automatic[F[_], A](a: A): CoAttr[F, A] = Automatic(a)
  def manual[F[_], A](fa: F[CoAttr[F, A]]): CoAttr[F, A] = Manual(fa)

  type CVCoalgebra[F[_], A] = A => F[CoAttr[F, A]]

  import cats.Functor
  import cats.implicits._
  def futu[F[_]: Functor, A](cvcoalgebra: CVCoalgebra[F, A])(a: A): Fix[F] = {
    def recv(coattr: CoAttr[F, A]): Fix[F] = coattr match {
      case Automatic(a) => futu(cvcoalgebra)(a)
      case Manual(fa) => Fix(fa.map(recv))
    }
    Fix(cvcoalgebra(a).map(recv))
  }

  def anaByFutu[F[_]: Functor, A](coalgebra: Coalgebra[F, A])(a: A): Fix[F] = {
    val cvcoalgebra: CVCoalgebra[F, A] = (a: A) => coalgebra(a).map(Automatic(_))
    futu(cvcoalgebra)(a)
  }

  private def fromFix[F[_]: Functor, A](fix: Fix[F]): CoAttr[F, A] = 
    Manual(fix.unfix.map(fx => fromFix(fx)))

  def apoByFutu[F[_]: Functor, A](rcoalgebra: RCoalgebra[F, A])(a: A): Fix[F] = {
    val cvcoalgebra: CVCoalgebra[F, A] = (a: A) => rcoalgebra(a).map {
      case Left(ctx) => fromFix(ctx)
      case Right(v) => Automatic(v)
    }
    futu(cvcoalgebra)(a)
  }
}