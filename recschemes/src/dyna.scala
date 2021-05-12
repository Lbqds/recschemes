package recschemes

import cats.Functor
import cats.implicits._

object dyna {

  def dyna[F[_]: Functor, A, C](cvalgebra: CVAlgebra[F, A])(coalgebra: C => F[C])(c: C): A = {
    def helper: C => Attr[F, A] = (c: C) => {
      val x = coalgebra(c).map(v => helper(v))
      Attr(cvalgebra(x), x)
    }
    helper(c).acc
  }

  final case class Point(x: Double, y: Double) {
    def distance(p: Point): Double = 
      math.sqrt(math.pow(x - p.x, 2) + math.pow(y - p.y, 2))
  }

  object Point {
    implicit val ordering: Ordering[Point] = new Ordering[Point] {
      override def compare(p1: Point, p2: Point): Int = 
        if (p1.x == p2.x) 0 else if (p1.x < p2.x) -1 else 1
    }
  }

  final case class Path(distance: Double, points: List[Point]) {
    def add(p: Point): Path = {
      val d = points.last.distance(p)
      Path(distance + d, points :+ p)
    }
  }
  
  object Path {
    implicit val ordering: Ordering[Path] = new Ordering[Path] {
      override def compare(p1: Path, p2: Path): Int = 
        if (p1.distance == p2.distance) 0 else if (p1.distance < p2.distance) -1 else 1
    }
  }

  def tour0(points: List[Point]): Path = {
    val mostLeft = points.min
    val mostRight = points.max
    val remains = points.toSet - mostLeft - mostRight

    def helper(nexts: Set[Point], func: Point => Option[Path]): Option[Path] = {
      val results = nexts.map(func).filter(_.isDefined)
      Option.when(results.nonEmpty)(results.map(_.get).min)
    }

    def goToRight(nexts: Set[Point], path: Path): Option[Path] =
      if (nexts.isEmpty)
        goToLeft(remains -- path.points, path.add(mostRight))
      else
        helper(nexts, next => goToRight(nexts.filter(_.x > next.x), path.add(next)))

    def goToLeft(nexts: Set[Point], path: Path): Option[Path] =
      if (nexts.isEmpty)
        Option.when(path.points.size == points.size)(path.add(mostLeft))
      else
        helper(nexts, next => goToLeft(nexts.filter(_.x < next.x), path.add(next)))

    goToRight(remains.filter(_.x > mostLeft.x), Path(0.0, List(mostLeft))).get
  }

  def tour1(points: List[Point]): Double = {
    import scala.collection.mutable
    val sortedPoints = points.sorted
    val distances = mutable.Map.empty[(Int, Int), Double]

    def distanceOf(i: Int, j: Int): Double =
      distances.getOrElseUpdate((i, j), sortedPoints(i).distance(sortedPoints(j)))

    def minimum(n: Int): Double = {
      if (n == 0) distanceOf(0, 1) * 2
      else (0 to (n - 1)).map(k =>
        minimum(k) - distanceOf(k, k + 1) + distanceOf(k, n + 1) + ((k + 1) to n).map(i => distanceOf(i, i + 1)).sum
      ).min
    }

    minimum(points.size - 2)
  }

  def tour2(points: List[Point]): Double = {
    import scala.collection.mutable
    val sortedPoints = points.sorted
    val distances = mutable.Map.empty[(Int, Int), Double]

    def distanceOf(i: Int, j: Int): Double =
      distances.getOrElseUpdate((i, j), sortedPoints(i).distance(sortedPoints(j)))

    // loop(i, j) expresses the minimal traversal of the points that starts at pi,
    // travels strictly left to p0, and then strictly right to pj
    // We assume that i <= j, and that all points smaller than j are in the
    // path. Clearly, when i == j we have a cycle: the final answer is to be
    // found in loop(m, m) when we have points(0..m). 
    // Precoditions:
    // 1. i <= j
    // 2. all points smaller than j are in the path
    // there must have a shortest path which satisfy these two conditions.
    def loop(i: Int, j: Int): Double = (i, j) match {
      case (0, 0) => 0
      case (0, 1) => distanceOf(0, 1)
      case (i, j) if i < j - 1 =>
        loop(i, j - 1) + distanceOf(j - 1, j)
      case (i, j) => (0 to (i - 1)).map(k =>
        loop(k, i) + distanceOf(k, j)
      ).min
    }

    loop(points.size - 1, points.size - 1)
  }

  import NonEmptyListF._
  // from `tour2`
  def tourByDyna(points: List[Point]): Double = {
    import scala.collection.mutable
    val sortedPoints = points.sorted
    val distances = mutable.Map.empty[(Int, Int), Double]

    def distanceOf(i: Int, j: Int): Double =
      distances.getOrElseUpdate((i, j), sortedPoints(i).distance(sortedPoints(j)))

    type Index = (Int, Int)
    val coalgebra: Coalgebra[NonEmptyListF[Index, *], Index] = {
      case (0, 0) => single((0, 0))
      case (0, 1) => single((0, 1))
      case (0, j) => Cons((0, j), (j - 1, j - 1))
      case (i, j) => Cons((i, j), (i - 1, j))
    }

    @scala.annotation.tailrec
    def lookup[A](cache: Attr[NonEmptyListF[Index, *], A], idx: Int): A = cache match {
      case Attr(a, _) if idx == 0 => a
      case Attr(_, Cons(_, as)) => lookup(as, idx - 1)
      case _ => ??? // never happen
    }


    // NonEmptyListF[Index, Attr[NonEmptyListF[Index], Double]] => Double
    val cvalgebra: CVAlgebra[NonEmptyListF[Index, *], Double] = {
      case Single((0, 0)) => 0
      case Single((0, 1)) => distanceOf(0, 1)
      case Cons((i, j), history) if i < j - 1 => lookup(history, j - 1) + distanceOf(j - 1, j)
      case Cons((i, j), history) if i == j - 1 => (0 to (i - 1)).map(k => lookup(history, k + j) + distanceOf(i - 1 - k, j)).min
      case Cons((i, j), history) if i == j => (0 to (i - 1)).map(k => lookup(history, k) + distanceOf(i - k - 1, j)).min
      case _ => ??? // never happen
    }
    dyna(cvalgebra)(coalgebra)((points.size - 1, points.size - 1))
  }
}

object DynaTest extends App {
  import dyna._
  
  val points = List(
    Point(1, 10),
    Point(4, 38),
    Point(18, 23),
    Point(9, 1),
    Point(10, 4),
    Point(45, 16),
    Point(3, 18),
    Point(7, 9),
  )
  println(tour0(points))
  println(tour1(points))
  println(tour2(points))
  println(tourByDyna(points))
}

