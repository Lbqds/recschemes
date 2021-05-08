package recschemes

import cats.Functor

sealed trait Nat[+A]

object Nat {
  final case object Zero extends Nat[Nothing]
  final case class Next[A](num: A) extends Nat[A]
  private def zero[A]: Nat[A] = Zero

  implicit val functor: Functor[Nat] =  new Functor[Nat] {
    override def map[A, B](fa: Nat[A])(f: A => B): Nat[B] = fa match {
      case Zero => Zero
      case Next(a) => Next(f(a))
    }
  }

  def fromInt(num: Int): Fix[Nat] = {
    if (num == 0) Fix(zero[Fix[Nat]])
    else Fix(Next(fromInt(num - 1)))
  }

  def fromIntByAna(num: Int): Fix[Nat] = {
    val coalgebra: Int => Nat[Int] = (n: Int) =>
      if (n == 0) zero else Next(n - 1)
    ana(coalgebra)(num)
  }

  def toInt(nat: Fix[Nat]): Int = nat.unfix match {
    case Zero => 0
    case Next(fa) => toInt(fa) + 1
  }

  def toIntByCata(nat: Fix[Nat]): Int = {
    val algebra: Algebra[Nat, Int] = {
      case Zero => 0
      case Next(num) => num + 1
    }
    cata(algebra)(nat)
  }
}

object NatTest extends App {
  import Nat._

  (100 to 200).foreach(num => assert(toInt(fromInt(num)) == num))
  (100 to 200).foreach(num => assert(toInt(fromIntByAna(num)) == num))
  (100 to 200).foreach(num => assert(toIntByCata(fromInt(num)) == num))
  (100 to 200).foreach(num => assert(toIntByCata(fromIntByAna(num)) == num))
}