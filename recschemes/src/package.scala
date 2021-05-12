import cats.Functor
import cats.implicits._
import cats.arrow.FunctionK

package object recschemes {

  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  // current ctx and accumulated value
  type RAlgebra[F[_], A] = Fix[F] => F[A] => A
  type RAlgebra1[F[_], A] = F[(Fix[F], A)] => A

  type RCoalgebra[F[_], A] = A => F[Either[Fix[F], A]]

  final case class Fix[F[_]](unfix: F[Fix[F]])

  // current accumulated value and history
  final case class Attr[F[_], A](acc: A, his: F[Attr[F, A]])

  type CVAlgebra[F[_], A] = F[Attr[F, A]] => A

  def cata[F[_]: Functor, A](algebra: Algebra[F, A])(fix: Fix[F]): A =
    algebra(fix.unfix.map(fa => cata(algebra)(fa)))

  def ana[F[_]: Functor, A](coalgebra: Coalgebra[F, A])(a: A): Fix[F] =
    Fix(coalgebra(a).map(v => ana(coalgebra)(v)))

  def para[F[_]: Functor, A](ralgebra: RAlgebra[F, A])(fix: Fix[F]): A =
    ralgebra(fix)(fix.unfix.map(fa => para(ralgebra)(fa)))

  def para1[F[_]: Functor, A](ralgebra: RAlgebra1[F, A])(fix: Fix[F]): A = {
    def f(fix: Fix[F]): (Fix[F], A) = (fix, para1(ralgebra)(fix))
    ralgebra(fix.unfix.map(f))
  }

  def apo[F[_]: Functor, A](rcoalgebra: RCoalgebra[F, A])(a: A): Fix[F] =
    Fix(rcoalgebra(a).map {
      case Left(ctx) => ctx
      case Right(v) => apo(rcoalgebra)(v)
    })

  def cataByPara[F[_]: Functor, A](algebra: Algebra[F, A])(fix: Fix[F]): A = {
    val ralgebra: RAlgebra[F, A] = (_: Fix[F]) => (fa: F[A]) => algebra(fa)
    para(ralgebra)(fix)
  }

  def anaByApo[F[_]: Functor, A](coalgebra: Coalgebra[F, A])(a: A): Fix[F] = {
    val rcoalgebra: RCoalgebra[F, A] = (a: A) => coalgebra(a).map(v => Right(v))
    apo(rcoalgebra)(a)
  }

  def prepro[F[_]: Functor, A, R](
    algebra: Algebra[F, A], 
    coalgebra: Coalgebra[F, R],
    f: FunctionK[F, F])(r: R): A = 
      algebra(f[R](coalgebra(r)).map(v => prepro(algebra, coalgebra, f)(v)))

  def postpro[F[_]: Functor, A, R](
    coalgebra: Coalgebra[F, A], 
    algebra: Algebra[F, R],
    f: FunctionK[F, F])(a: A): R = 
      algebra(f[A](coalgebra(a)).map(v => postpro(coalgebra, algebra, f)(v)))
}