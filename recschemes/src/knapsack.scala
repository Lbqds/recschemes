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

  @scala.annotation.tailrec
  private def lookup[A](cache: Attr[Nat, A], idx: Int): A = {
    if (idx == 0) cache.acc
    else cache.his match {
      case Next(attr) => lookup(attr, idx - 1)
      case _ => throw new IllegalAccessException(s"lookup index $idx large than cache size")
    }
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

  @scala.annotation.tailrec
  private def lookup1[A](cache: Attr[Nat, A], idx: Int): Option[A] = {
    if (idx == 0) Some(cache.acc)
    else cache.his match {
      case Next(attr) => lookup1(attr, idx - 1)
      case _ => None
    }
  }

  // from the paper: histo and dynamorphisms revisited
  def knapsackByHisto1(capacity: Int, items: List[Item]): Double = {
    val cvalgebra: CVAlgebra[Nat, Double] = {
      case Zero => 0.0
      case Next(cache) => items.map(item =>
        // for eache Item(w, v), what we want now is maximum(capacity - w), but lookup would search in reverse order. 
        // so the index is capacity - 1 - (capacity - w), `-1` because of we start from current which index is 0
        lookup1(cache, item.weight - 1).map(_ + item.value).getOrElse(0.0)
      ).max
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
  (1 to 40).foreach(capacity => assert(knapsackWithCache(capacity, items) == knapsackByHisto1(capacity, items)))
}