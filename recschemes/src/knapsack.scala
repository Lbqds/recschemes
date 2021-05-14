package recschemes

import cats.Functor
import scala.collection.mutable
import Nat._

object Knapsack {
  import histo._

  private def toInt[A](nat: Nat[Attr[Nat, A]]): Int = nat match {
    case Zero => 0
    case Next(attr) => toInt(attr.his) + 1
  }

  @scala.annotation.tailrec
  private def lookup[A](cache: Attr[Nat, A], idx: Int): A = {
    if (idx == 0) cache.acc
    else cache.his match {
      case Next(attr) => lookup(attr, idx - 1)
      case _ => throw new IllegalAccessException(s"lookup index $idx large than cache size")
    }
  }

  final case class Item(weight: Int, value: Double)

  def knapsack(capacity: Int, items: List[Item]): Double = {
    val results = items.filter(_.weight <= capacity).map(item => item.value + knapsack(capacity - item.weight, items))
    if (results.isEmpty) 0.0 else results.max
  }

  def knapsackWithCache(capacity: Int, items: List[Item]): Double = {
    val cache = mutable.Map.empty[Int, Double]
    
    def maximum(capacity: Int): Double = {
      val results = items.filter(_.weight <= capacity).map { item =>
        val remain = cache.getOrElseUpdate(capacity - item.weight, maximum(capacity - item.weight))
        item.value + remain
      }
      if (results.isEmpty) 0.0 else results.max
    }

    maximum(capacity)
  }

  def knapsackByHisto(capacity: Int, items: List[Item]): Double = {
    val cvalgebra: CVAlgebra[Nat, Double] = {
      case Zero => 0.0
      case nat @ Next(attr) =>
        val current = toInt(nat)
        val results = items.filter(_.weight <= current).map { item =>
          val remain = current - item.weight
          lookup(attr, current - 1 - remain) + item.value
        }
        if (results.isEmpty) 0.0 else results.max
    }
    histo(cvalgebra)(fromIntByAna(capacity))
  }
}

object KnapsackTest extends App {
  import Knapsack._

  private val items = List(
    Item(12, 4),
    Item(2, 2),
    Item(1, 2),
    Item(1, 1),
    Item(4, 10)
  )

  (1 to 15).foreach(capacity => assert(knapsack(capacity, items) == knapsackWithCache(capacity, items)))
  (1 to 40).foreach(capacity => assert(knapsackWithCache(capacity, items) == knapsackByHisto(capacity, items)))
}