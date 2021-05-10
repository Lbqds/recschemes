package recschemes

object histo {
  // current accumulated value and history
  final case class Attr[F[_], A](acc: A, his: F[Attr[F, A]])

  type CVAlgebra[F[_], A] = F[Attr[F, A]] => A

  import cats.Functor
  import cats.implicits._

  /* repeated computation
  def histo[F[_]: Functor, A](cvalgebra: CVAlgebra[F, A])(fix: Fix[F]): A = {
    def recv(fa: Fix[F]): Attr[F, A] =
      Attr(histo(cvalgebra)(fa), fa.unfix.map(recv))

    cvalgebra(fix.unfix.map(recv))
  }
   */

  def dyna[F[_]: Functor, A, C](cvalgebra: CVAlgebra[F, A])(coalgebra: C => F[C])(c: C): A = {
    def helper: C => Attr[F, A] = (c: C) => {
      val x = coalgebra(c).map(v => helper(v))
      Attr(cvalgebra(x), x)
    }
    helper(c).acc
  }

  def histo[F[_]: Functor, A](cvalgebra: CVAlgebra[F, A])(fix: Fix[F]): A = {
    def recv(fa: Fix[F]): Attr[F, A] = {
      val res = fa.unfix.map(recv)
      Attr(cvalgebra(res), res)
    }

    recv(fix).acc
  }

  def cataByHisto[F[_]: Functor, A](algebra: Algebra[F, A])(fix: Fix[F]): A = {
    val cvalgebra: CVAlgebra[F, A] = fattr => algebra(fattr.map(_.acc))
    histo(cvalgebra)(fix)
  }

  private def toFix[F[_]: Functor, A](attr: Attr[F, A]): (Fix[F], A) =
    (Fix(attr.his.map(fattr => toFix(fattr)._1)), attr.acc)

  def paraByHisto[F[_]: Functor, A](ralgebra: RAlgebra1[F, A])(fix: Fix[F]): A = {
    val cvalgebra: CVAlgebra[F, A] = fattr => {
      ralgebra(fattr.map(attr => toFix(attr)))
    }
    histo(cvalgebra)(fix)
  }
}