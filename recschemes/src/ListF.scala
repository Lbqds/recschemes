package recschemes

sealed trait ListF[E, A]

object ListF {
  final case class Nil[E, A]() extends ListF[E, A]
  final case class Cons[E, A](v: E, t: A) extends ListF[E, A]

  import cats.Functor
  implicit def functor[E]: Functor[ListF[E, *]] = new Functor[ListF[E, *]] {
    override def map[A, B](fa: ListF[E, A])(f: A => B): ListF[E, B] = fa match {
      case Nil() => Nil()
      case Cons(v, t) => Cons(v, f(t))
    }
  }

  def fold[E, T](init: T)(f: (E, T) => T): Fix[ListF[E, *]] => T = {
    val algebra: Algebra[ListF[E, *], T] = {
      case Nil() => init
      case Cons(v, t) => f(v, t)
    }
    cata(algebra)
  }
}

object ListFTest extends App {
  import ListF._

  private val lst: Fix[ListF[Int, *]] = Fix(Cons(2, Fix(Cons(3, Fix(Cons(4, Fix(Cons(5, Fix(Nil())))))))))
  assert(fold[Int, Int](0)((a, b) => a + b)(lst) == 14)
  assert(fold[Int, String]("")((a, b) => a.toString + b)(lst) == "2345")
  assert(fold[Int, Int](1)((a, b) => a * b)(lst) == 120)
}