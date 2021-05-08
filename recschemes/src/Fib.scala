package recschemes

object Fib {
  import Nat._
  import histo._

  def fibByHisto(nat: Fix[Nat]): Int = {
    val cvalgebra: CVAlgebra[Nat, Int] = {
      case Zero => 0
      case Next(Attr(0, his)) => 1
      case Next(Attr(acc, his)) => his match {
        case Next(Attr(prev, _)) => acc + prev
        case Zero => ???   // never happen
      }
    }
    histo(cvalgebra)(nat)
  }

  def fib: Int => Int = {
    case 0 => 0
    case 1 => 1
    case n => fib(n - 1) + fib(n - 2)
  }
}

object FibTest extends App {
  import Fib._
  import Nat._

  (0 to 30).foreach(num => assert(fibByHisto(fromInt(num)) == fib(num)))
}