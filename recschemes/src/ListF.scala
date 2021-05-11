package recschemes

sealed trait ListF[E, A]

object ListF {
  final case class Nil[E, A]() extends ListF[E, A]
  final case class Cons[E, A](v: E, t: A) extends ListF[E, A]

  def nil[E, A]: ListF[E, A] = Nil[E, A]()
  def apply[E](lst: List[E]): Fix[ListF[E, *]] = {
    val f: List[E] => ListF[E, List[E]] = {
      case h :: t => Cons(h, t)
      case scala.collection.immutable.Nil => nil[E, List[E]]
    }
    ana(f)(lst)
  }

  /* reverse
  def index[E](lst: Fix[ListF[E, *]], i: Int): Option[E] = {
    val f: Algebra[ListF[E, *], (Int, Option[E])] = {
      case Nil() => (0, None)
      case Cons(curr, (size, res)) if size == i && res.isEmpty => (i, Some(curr))
      case Cons(_, (size, res)) => (size + 1, res)
    }
    cata(f)(lst)._2
  }
  */

  def toList[E](fix: Fix[ListF[E, *]]): List[E] = {
    val f: ListF[E, List[E]] => List[E] = {
      case Nil() => scala.collection.immutable.Nil
      case Cons(e, acc) => e +: acc
    }
    cata[ListF[E, *], List[E]](f)(fix)
  }

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

  val list = List(1, 2, 3, 4)
  assert(toList(ListF[Int](list)) == list)
}