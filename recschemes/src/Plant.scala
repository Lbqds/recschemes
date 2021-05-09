package recschemes

sealed trait Plant[+A]

object Plant {
  final case class Root[A](v: A) extends Plant[A]
  final case class Stalk[A](v: A) extends Plant[A]
  final case class Fork[A](left: A, mid: A, right: A) extends Plant[A]
  final case object Bloom extends Plant[Nothing]

  def root[A](v: A): Plant[A] = Root(v)
  def stalk[A](v: A): Plant[A] = Stalk(v)
  def fork[A](l: A, m: A, r: A): Plant[A] = Fork(l, m, r)
  private def bloom[A]: Plant[A] = Bloom

  import cats.Functor
  import cats.implicits._
  implicit val functor: Functor[Plant] = new Functor[Plant] {
    override def map[A, B](fa: Plant[A])(f: A => B): Plant[B] = fa match {
      case Root(v) => Root(f(v))
      case Stalk(v) => Stalk(f(v))
      case Fork(left, mid, right) => Fork(f(left), f(mid), f(right))
      case Bloom => Bloom
    }
  }

  sealed trait Action
  final case object Flower extends Action
  final case object Upwards extends Action
  final case object Branch extends Action

  import scala.util.Random
  final case class Seed(height: Int, rnd: Random)

  def nextAction(random: Random): Action = {
    val value = random.between(1, 6)
    println(value)
    value match {
      case 1 => Flower
      case 2 => Upwards
      case _ => Branch
    }
  }

  import futu._
  def build(maxHeight: Int)(random: Random): Fix[Plant] = {
    val cvcoalgebra: CVCoalgebra[Plant, Int] = (h: Int) => h match {
      case 0 => root(automatic[Plant, Int](1))
      case `maxHeight` => bloom
      case _ => nextAction(random) match {
        case Flower => bloom
        case Upwards => stalk(automatic[Plant, Int](h + 1))
        case Branch => fork(
          manual[Plant, Int](stalk(automatic[Plant, Int](h + 1))), 
          manual[Plant, Int](bloom), 
          manual[Plant, Int](stalk(automatic[Plant, Int](h + 1))))
      }
    }
    futu(cvcoalgebra)(0)
  }

  // maximum branch size from flower to root
  def branchs(plant: Fix[Plant]): Int = {
    def h: Plant[Int] => Int = {
      case Bloom => 0
      case Fork(l, _, r) => math.max(l, r) + 1
      case Root(v) => v
      case Stalk(v) => v
    }
    cata[Plant, Int](h)(plant)
  }

  def render(plant: Fix[Plant]): String = {
    def p: Plant[String] => String = {
      case Bloom => "bloom"
      case Fork(l, m, r) => s"fork($l, $m, $r)"
      case Stalk(v) => s"stalk($v)"
      case Root(v) => s"root($v)"
    }
    cata[Plant, String](p)(plant)
  }
}

object PlantTest extends App {
  import Plant._
  
  val plant = build(10)(new scala.util.Random)
  println(render(plant))
}