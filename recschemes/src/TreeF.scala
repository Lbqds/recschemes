package recschemes

sealed trait TreeF[E, A]

object TreeF {
  final case class Leaf[E, A](v: E) extends TreeF[E, A]
  final case class Branch[E, A](l: A, r: A) extends TreeF[E, A]

  import cats.Functor
  implicit def functor[E]: Functor[TreeF[E, *]] = new Functor[TreeF[E, *]] {
    override def map[A, B](fa: TreeF[E,A])(f: A => B): TreeF[E,B] = fa match {
      case Leaf(v) => Leaf(v)
      case Branch(l, r) => Branch(f(l), f(r))
    }
  }
}