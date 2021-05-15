package recschemes

object histo {
  import cats.Functor
  import cats.implicits._

  /* repeated computation
  def histo[F[_]: Functor, A](cvalgebra: CVAlgebra[F, A])(fix: Fix[F]): A = {
    def recv(fa: Fix[F]): Attr[F, A] =
      Attr(histo(cvalgebra)(fa), fa.unfix.map(recv))

    cvalgebra(fix.unfix.map(recv))
  }
   */

  def histo[F[_]: Functor, A](cvalgebra: CVAlgebra[F, A])(fix: Fix[F]): A = {
    def recv(fa: Fix[F]): Attr[F, A] = {
      val res = fa.unfix.map(recv)
      Attr(cvalgebra(res), res)
    }

    recv(fix).acc
  }

  def histoByCata[F[_]: Functor, A](cvalgebra: CVAlgebra[F, A])(fix: Fix[F]): A = {
    val algebra: Algebra[F, Attr[F, A]] = (fattr: F[Attr[F, A]]) => Attr(cvalgebra(fattr), fattr)
    cata(algebra)(fix).acc
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