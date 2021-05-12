package recschemes

sealed trait NonEmptyListF[A, B]

object NonEmptyListF {
  final case class Single[A, B](v: A) extends NonEmptyListF[A, B]
  final case class Cons[A, B](v: A, t: B) extends NonEmptyListF[A, B]

  def single[A, B](v: A): NonEmptyListF[A, B] = Single(v)

  import cats.Functor
  implicit def functor[E]: Functor[NonEmptyListF[E, *]] = new Functor[NonEmptyListF[E, *]] {
    override def map[A, B](fa: NonEmptyListF[E,A])(f: A => B): NonEmptyListF[E,B] = fa match {
      case Single(v) => Single(v)
      case Cons(v, t) => Cons(v, f(t))
    }
  }
}