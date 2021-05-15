package recschemes

sealed trait ListF[+E, +A]

object ListF {
  final case object NilF extends ListF[Nothing, Nothing]
  final case class ConsF[E, A](v: E, t: A) extends ListF[E, A]

  def nil[E, A]: ListF[E, A] = NilF
  def apply[E](lst: List[E]): Fix[ListF[E, *]] = ana(listFCoalgebra[E])(lst)

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

  def toList[E](fix: Fix[ListF[E, *]]): List[E] = cata[ListF[E, *], List[E]](listFAlgebra[E])(fix)

  def listFAlgebra[E]: Algebra[ListF[E, *], List[E]] = {
    case NilF => Nil
    case ConsF(e, acc) => e +: acc
  }

  def listFCoalgebra[E]: Coalgebra[ListF[E, *], List[E]] = {
    case Nil => nil
    case h :: t => ConsF(h, t)
  }

  import cats.Functor
  implicit def functor[E]: Functor[ListF[E, *]] = new Functor[ListF[E, *]] {
    override def map[A, B](fa: ListF[E, A])(f: A => B): ListF[E, B] = fa match {
      case NilF => nil
      case ConsF(v, t) => ConsF(v, f(t))
    }
  }

  def fold[E, T](init: T)(f: (E, T) => T): Fix[ListF[E, *]] => T = {
    val algebra: Algebra[ListF[E, *], T] = {
      case NilF => init
      case ConsF(v, t) => f(v, t)
    }
    cata(algebra)
  }
}

object ListFTest extends App {
  import ListF._

  private val lst: Fix[ListF[Int, *]] = Fix(ConsF(2, Fix(ConsF(3, Fix(ConsF(4, Fix(ConsF(5, Fix(nil[Int, Fix[ListF[Int, *]]])))))))))
  assert(fold[Int, Int](0)((a, b) => a + b)(lst) == 14)
  assert(fold[Int, String]("")((a, b) => a.toString + b)(lst) == "2345")
  assert(fold[Int, Int](1)((a, b) => a * b)(lst) == 120)

  val list = List(1, 2, 3, 4)
  assert(toList(ListF[Int](list)) == list)

  // ===================== prepro & postpro begin ===================
  import cats.arrow.FunctionK
  def transformer(num: Int, less: Boolean)  = new FunctionK[ListF[Int, *], ListF[Int, *]] {
    override def apply[A](fa: ListF[Int,A]): ListF[Int,A] = fa match {
      case NilF => nil
      case c @ ConsF(h, _) if less && h < num => c
      case c @ ConsF(h, _) if !less && h > num => c
      case _ => nil
    }
  }

  def sumAlgebra: Algebra[ListF[Int, *], Int] = {
    case NilF => 0
    case ConsF(a, b) => a + b
  }
  assert(prepro(sumAlgebra, listFCoalgebra[Int], transformer(7, true))(List(4, 5, 6, 7, 8, 1)) == 15)

  def initCoalgebra: Coalgebra[ListF[Int, *], Int] = {
    case 0 => nil
    case n => ConsF(n, n - 1)
  }
  assert(postpro(initCoalgebra, listFAlgebra[Int], transformer(7, false))(10) == List(10, 9, 8))

  // ===================== prepro & postpro end ===================

  // ===================== sliding window by para begin ===================

  def slidingWindows[T](lst: List[T], size: Int): List[List[T]] = {
    // ListF[T, (Fix[ListF[T, *]], List[List[T]])] => List[List[T]]
    val ralgebra: RAlgebra1[ListF[T, *], List[List[T]]] = {
      case NilF => List.empty[List[T]]
      case ConsF(v, (curr, acc)) => 
        val lst = toList(curr)
        if (lst.size < size - 1) acc
        else (v +: lst.take(size - 1)) +: acc
    }
    para1(ralgebra)(ListF(lst))
  }

  assert(slidingWindows(List(1, 2, 3, 4, 5), 3) == List(List(1, 2, 3), List(2, 3, 4), List(3, 4, 5)))
  assert(slidingWindows(List(1, 2, 3, 4, 5), 6) == List.empty[List[Int]])

  // ===================== sliding window by para end ===================

  // ===================== insert sort by cata and apo begin ===================

  import scala.math.Ordering.Implicits._
  def insertElement[T: Ordering]: Algebra[ListF[T, *], List[T]] = {
    // ListF[T, List[T]] => ListF[T, Either[Fix[ListF[T, *]], ListF[T, List[T]]]]
    val rcoalgebra: RCoalgebra[ListF[T, *], ListF[T, List[T]]] = {
      case NilF => nil
      case ConsF(v, t) if t.isEmpty => ConsF(v, Right(nil))
      case ConsF(v, l @ (h :: t)) if v <= h => ConsF(v, Left(ListF[T](l)))
      case ConsF(v, l @ (h :: t)) if v > h => ConsF(h, Right(ConsF(v, t)))
      case _ => ??? // never happen
    }
    (lst: ListF[T, List[T]]) => toList(apo(rcoalgebra)(lst))
  }

  def insertSort[T: Ordering](lst: List[T]): List[T] = cata(insertElement[T])(ListF[T](lst))

  assert(insertSort(List(12, 5, 7, 89, 23, 0, 34, 9)) == List(0, 5, 7, 9, 12, 23, 34, 89))

  // ===================== insert sort by cata and apo begin ===================
}