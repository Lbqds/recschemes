package recschemes

import scala.util.control.NonFatal
import cats.Functor
import cats.implicits._

object hylo {
  def hylo[F[_]: Functor, A, B](algebra: Algebra[F, B])(coalgebra: Coalgebra[F, A])(a: A): B =
    cata(algebra)(ana(coalgebra)(a))

  sealed trait Token
  final case class Lit(value: Int) extends Token
  final case class Op(func: (Int, Int) => Int) extends Token

  def parseToken(str: String): Token = str match {
    case "+" => Op((a, b) => a + b)
    case "-" => Op((a, b) => a - b)
    case "*" => Op((a, b) => a * b)
    case "/" => Op((a, b) => a / b)
    case _ => Lit(str.toInt)
  }

  def parse(str: String): ListF[Token, String] = {
    if (str.isEmpty) ListF.nil[Token, String] else {
      val concrete = str.dropWhile(_.isWhitespace)
      val (current, remain) = concrete.span(c => !c.isWhitespace)
      ListF.Cons(parseToken(current), remain)
    }
  }
  
  // Algebra[ListF[Token, *], List[Int] => List[Int]]
  // expr = "2 3 +", we get a function:
  // func: List[Int] => List[Int] = (lst: List[Int]) => cont3(2 +: lst) where
  // cont3 = (lst: List[Int]) => cont2(3 +: lst)
  // cont2 = (lst: List[Int]) => cont1(lst match {
  //   case (a :: b :: tail) => (b + a) +: tail
  //   case _ => error
  // })
  // cont1 = id[List[Int]]
  //
  // then we apply `func` with empty list `func(List.empty[Int])` and get the result: [5]
  def evaluate(expr: ListF[Token, List[Int] => List[Int]]): List[Int] => List[Int] = expr match {
    case ListF.Nil() => (lst: List[Int]) => lst
    case ListF.Cons(Lit(value), cont) => (lst: List[Int]) => cont(value +: lst)
    case ListF.Cons(Op(func), cont) => (lst: List[Int]) => cont(lst match {
      case (a :: b :: tail) => func(b, a) +: tail
      case _ => throw new RuntimeException(s"no enough operands, stack: $lst")
    })
  }
  
  def rpn(exprStr: String): List[Int] = {
    val algebra: Algebra[ListF[Token, *], List[Int] => List[Int]] = evaluate(_)
    val coalgebra: Coalgebra[ListF[Token, *], String] = parse(_)
    val func = hylo(algebra)(coalgebra)(exprStr)
    func(List.empty[Int])
  }

  def elgot[F[_]: Functor, A, B](algebra: Algebra[F, B])(coalgebra: A => Either[B, F[A]])(a: A): B =
    coalgebra(a) match {
      case Left(b) => b
      case Right(fa) => algebra(fa.map(v => elgot(algebra)(coalgebra)(v)))
    }

  def coelgot[F[_]: Functor, A, B](algebra: (A, F[B]) => B)(coalgebra: A => F[A])(a: A): B = 
    algebra(a, coalgebra(a).map(v => coelgot(algebra)(coalgebra)(v)))

  // can't implement hypo directly by `ralgebra` and `rcoalgebra`
  def hypo[F[_]: Functor, A, B](ralgebra: RAlgebra[F, B])(rcoalgebra: RCoalgebra[F, A])(a: A): B =
    para(ralgebra)(apo(rcoalgebra)(a))

  sealed trait Result
  final case class Succeed(value: List[Int]) extends Result {
    override def toString: String = s"Succeed($value)"
  }
  final case class ParseError(msg: String) extends Result {
    override def toString: String = s"ParseError($msg)"
  }
  final case class NoEnoughOperands(stack: List[Int]) extends Result {
    override def toString: String = s"NoEnoughOperands($stack)"
  }

  type Cont = Result => Result
  def safeParseToken(str: String): Either[Cont, Token] = str match {
    case "+" => Right(Op((a, b) => a + b))
    case "-" => Right(Op((a, b) => a - b))
    case "*" => Right(Op((a, b) => a * b))
    case "/" => Right(Op((a, b) => a / b))
    case _ => try Right(Lit(str.toInt)) catch {
      case NonFatal(exception) => Left(_ => ParseError(exception.getMessage))
    }
  }

  def safeParse(str: String): Either[Cont, ListF[Token, String]] = {
    if (str.isEmpty) Right(ListF.nil[Token, String]) else {
      val concrete = str.dropWhile(_.isWhitespace)
      val (current, remain) = concrete.span(c => !c.isWhitespace)
      safeParseToken(current).map(token => ListF.Cons(token, remain))
    }
  }

  def safeEvaluate: ListF[Token, Cont] => Cont = (expr: ListF[Token, Cont]) => (res: Result) => (expr, res) match {
    case (ListF.Cons(Lit(value), cont), Succeed(lst)) => cont(Succeed(value +: lst))
    case (ListF.Cons(Op(func), cont), Succeed(lst)) => cont(lst match {
      case (a :: b :: tail) => Succeed(func(b, a) +: tail)
      case _ => NoEnoughOperands(lst)
    })
    case (_, result) => result
  }
  
  def safeRpn(exprStr: String): Result = {
    val algebra: Algebra[ListF[Token, *], Cont] = safeEvaluate
    val coalgebra: String => Either[Cont, ListF[Token, String]] = safeParse(_)
    val func = elgot(algebra)(coalgebra)(exprStr)
    func(Succeed(List.empty[Int]))
  }

  // merge sort by hylo
  import scala.math.Ordering.Implicits._
  private def mergeLists[T: Ordering](l: List[T], r: List[T]): List[T] = (l, r) match {
    case (Nil, r) => r
    case (l, Nil) => l
    case (lh :: lt, rh :: rt) => if (lh < rh) lh +: mergeLists(lt, r) else rh +: mergeLists(l, rt)
  }

  import cats.data.NonEmptyList
  def mergeSort[T: Ordering](lst: NonEmptyList[T]): List[T] = {
    val coalgebra: Coalgebra[TreeF[T, *], List[T]] = {
      case Nil => ??? // never happen
      case v :: Nil => TreeF.Leaf(v)
      case lst => 
        val (l, r) = lst.splitAt(lst.size / 2)
        TreeF.Branch(l, r)
    }
    val algebra: Algebra[TreeF[T, *], List[T]] = {
      case TreeF.Leaf(v) => List(v)
      case TreeF.Branch(l, r) => mergeLists(l, r)
    }
    hylo(algebra)(coalgebra)(lst.toList)
  }
}

object HyloTest extends App {
  import hylo._

  assert(rpn("2 3 +") == List(5))
  assert(rpn("15 7 1 1 + - / 3 * 2 1 1 + + -") == List(5))

  assert(safeRpn("2 3 +") == Succeed(List(5)))
  assert(safeRpn("15 7 1 1 + - / 3 * 2 1 1 + + -") == Succeed(List(5)))
  assert(safeRpn("2 3 4 +") == Succeed(List(7, 2)))
  assert(safeRpn("2 +") == NoEnoughOperands(List(2)))
  assert(safeRpn("a 1 3 +").isInstanceOf[ParseError])

  import cats.data.NonEmptyList
  assert(mergeSort(NonEmptyList.fromListUnsafe(List(12, 5, 7, 89, 23, 0, 34, 9))) == List(0, 5, 7, 9, 12, 23, 34, 89))
}
