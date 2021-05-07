package recschemes

// implicit recursive with catamorphism
sealed trait ExprF[A]

object ExprF {
  final case class Call[A](func: A, args: List[A]) extends ExprF[A]
  final case class Index[A](target: A, idx: A) extends ExprF[A]
  final case class Unary[A](op: String, arg: A) extends ExprF[A]
  final case class Binary[A](left: A, op: String, right: A) extends ExprF[A]
  final case class Paren[A](inner: A) extends ExprF[A]
  final case class Literal[A](raw: Int) extends ExprF[A]
  final case class Ident[A](name: String) extends ExprF[A]

  private def nodeCount(expr: ExprF[Int]): Int = expr match {
    case Call(func, args) => func + args.sum + 1
    case Index(t, i) => t + i + 1
    case Unary(_, arg) => arg + 1
    case Binary(l, _, r) => l + r + 1
    case Paren(inner) => inner + 1
    case Literal(_) | Ident(_) => 1
  }

  import cats.Functor
  // can be auto derived
  implicit val functor: Functor[ExprF] = new Functor[ExprF] {
    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case Call(func, args) => Call(f(func), args.map(f))
      case Index(target, idx) => Index(f(target), f(idx))
      case Unary(op, arg) => Unary(op, f(arg))
      case Binary(left, op, right) => Binary(f(left), op, f(right))
      case Paren(inner) => Paren(f(inner))
      case v @ (Literal(_) | Ident(_)) => v.asInstanceOf[ExprF[B]]
    }
  }

  def nodeCount: Fix[ExprF] => Int = cata[ExprF, Int](nodeCount)
  def nodeCountByPara: Fix[ExprF] => Int = cataByPara[ExprF, Int](nodeCount)

  import org.typelevel.paiges.Doc
  def prettyPrint(expr: ExprF[Doc]): Doc = expr match {
    case Call(func, args) => func + Doc.char('(') + Doc.fill(Doc.comma, args) + Doc.char(')')
    case Index(t, i) => t + Doc.char('[') + i + Doc.char(']')
    case Unary(op, arg) => Doc.text(op) + arg
    case Binary(l, op, r) => l + Doc.space + Doc.text(op) + Doc.space + r
    case Paren(inner) => Doc.char('(') + inner + Doc.char(')')
    case Literal(v) => Doc.str(v)
    case Ident(name) => Doc.text(name)
  }

  def prettyPrint: Fix[ExprF] => Doc = cata[ExprF, Doc](prettyPrint)
  def prettyPrintByPara: Fix[ExprF] => Doc = cataByPara[ExprF, Doc](prettyPrint)

  def nestedExpr: Int => Fix[ExprF] = (n: Int) => {
    val coalgebra: Coalgebra[ExprF, Int] = (x: Int) => {
      if (x == 0) Literal(n)
      else Paren(x - 1)
    }
    ana(coalgebra)(n)
  }

  def nestedExprByApo: Int => Fix[ExprF] = (n: Int) => {
    val coalgebra: Coalgebra[ExprF, Int] = (x: Int) => {
      if (x == 0) Literal(n)
      else Paren(x - 1)
    }
    anaByApo(coalgebra)(n)
  }

  private def prettyPrintIgnoreId(ctx: Fix[ExprF], expr: ExprF[Doc]): Doc = (ctx, expr) match {
    // if current context is `id` function call and the number of args is 1, ignore the `id`
    case (Fix(Call(Fix(Ident("id")), _)), Call(_, args)) if args.size == 1 => args.head
    case (_, Call(func, args)) => func + Doc.char('(') + Doc.fill(Doc.comma, args) + Doc.char(')')
    case (_, Index(t, i)) => t + Doc.char('[') + i + Doc.char(']')
    case (_, Unary(op, arg)) => Doc.text(op) + arg
    case (_, Binary(l, op, r)) => l + Doc.space + Doc.text(op) + Doc.space + r
    case (_, Paren(inner)) => Doc.char('(') + inner + Doc.char(')')
    case (_, Literal(v)) => Doc.str(v)
    case (_, Ident(name)) => Doc.text(name)
  }

  def prettyPrintIgnoreId: Fix[ExprF] => Doc =
    para[ExprF, Doc]((ctx: Fix[ExprF]) => (expr: ExprF[Doc]) => prettyPrintIgnoreId(ctx, expr))
}

object ExprFText extends App {
  import ExprF._

  private val ten: Fix[ExprF] = Fix(Literal(10))
  private val add: Fix[ExprF] = Fix(Ident("add"))
  private val addCall: Fix[ExprF] = Fix(Call(add, List(ten, ten)))

  assert(nodeCount(addCall) == nodeCountByPara(addCall))
  assert(prettyPrint(addCall).render(60) == prettyPrintByPara(addCall).render(60))
  assert(prettyPrint(nestedExpr(5)).render(60) == prettyPrint(nestedExprByApo(5)).render(60))

  private val variable: Fix[ExprF] = Fix(Ident("foo"))
  private val idFunc: Fix[ExprF] = Fix(Ident("id"))
  private val idCall: Fix[ExprF] = Fix(Call(idFunc, List(variable)))
  println(prettyPrint(idCall).render(60))
  println(prettyPrintIgnoreId(idCall).render(60))
}