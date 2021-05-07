import cats.Functor
import cats.implicits._

package object recschemes {

  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]
  // current ctx and accumulated value, same as `F[(Fix[F], A)] => A`
  type RAlgebra[F[_], A] = Fix[F] => F[A] => A
  type RCoalgebra[F[_], A] = A => Either[Fix[F], F[A]]

  final case class Fix[F[_]](unfix: F[Fix[F]])

  def cata[F[_]: Functor, A](algebra: Algebra[F, A])(fix: Fix[F]): A =
    algebra(fix.unfix.map(fa => cata(algebra)(fa)))

  def ana[F[_]: Functor, A](coalgebra: Coalgebra[F, A])(a: A): Fix[F] =
    Fix(coalgebra(a).map(v => ana(coalgebra)(v)))

  def para[F[_]: Functor, A](ralgebra: RAlgebra[F, A])(fix: Fix[F]): A =
    ralgebra(fix)(fix.unfix.map(fa => para(ralgebra)(fa)))

  def apo[F[_]: Functor, A](rcoalgebra: RCoalgebra[F, A])(a: A): Fix[F] =
    rcoalgebra(a) match {
      case Left(ctx) => ctx
      case Right(fa) => Fix(fa.map(v => apo(rcoalgebra)(v)))
    }

  def cataByPara[F[_]: Functor, A](algebra: Algebra[F, A])(fix: Fix[F]): A = {
    val ralgebra: RAlgebra[F, A] = (_: Fix[F]) => (fa: F[A]) => algebra(fa)
    para(ralgebra)(fix)
  }

  def anaByApo[F[_]: Functor, A](coalgebra: Coalgebra[F, A])(a: A): Fix[F] = {
    val rcoalgebra: RCoalgebra[F, A] = (a: A) => Right(coalgebra(a))
    apo(rcoalgebra)(a)
  }
}