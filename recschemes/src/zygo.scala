package recschemes

object zygo {
  import cats.Functor
  import cats.implicits._
  
  def zygoNaive[F[_]: Functor, A, B](algebra: Algebra[F, B])(func: F[(B, A)] => A)(fix: Fix[F]): A =
    func(fix.unfix.map(fa => (cata(algebra)(fa), zygo(algebra)(func)(fa))))

  def zygo[F[_]: Functor, A, B](algebra: Algebra[F, B])(func: F[(B, A)] => A)(fix: Fix[F]): A = {
    val f: F[(B, A)] => (B, A) = (x: F[(B, A)]) => (algebra(x.map(_._1)), func(x))
    cata[F, (B, A)](f)(fix)._2
  }

  type ZygoImpl[F[_], A, B] = Algebra[F, B] => (F[(B, A)] => A) => Fix[F] => A

  // foo [a, b, c, d, e] = a - b + c - d + e
  private def foo(lst: Fix[ListF[Int, *]])(zygoImpl: ZygoImpl[ListF[Int, *], Int, Boolean]): Int = {
    val algebra: ListF[Int, Boolean] => Boolean = {
      case ListF.Nil() => false
      case ListF.Cons(_, b) => !b
    }
    val func: ListF[Int, (Boolean, Int)] => Int = {
      case ListF.Nil() => 0
      case ListF.Cons(curr, (b, acc)) => if (b) acc + curr else acc - curr
    }
    zygo[ListF[Int, *], Int, Boolean](algebra)(func)(lst)
  }

  def foo(lst: List[Int]): Int = foo(ListF[Int](lst))(zygo)

  def fooNaive(lst: List[Int]): Int = foo(ListF[Int](lst))(zygoNaive)
}

object ZygoTest extends App {
  import zygo._
  assert(foo(List(1, 2, 3, 4)) == -2)
  assert(foo(List(4, 3, 2, 1)) == 2)
  assert(foo(List(1, 2, 3, 4)) == fooNaive(List(1, 2, 3, 4)))
  assert(foo(List(4, 3, 2, 1)) == fooNaive(List(4, 3, 2, 1)))
}