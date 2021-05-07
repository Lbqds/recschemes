package recschemes

// explicit recursive
sealed trait Expr

object Expr {
  final case class Call(func: Expr, args: List[Expr]) extends Expr
  final case class Index(target: Expr, idx: Expr) extends Expr
  final case class Unary(op: String, arg: Expr) extends Expr
  final case class Binary(left: Expr, op: String, right: Expr) extends Expr
  final case class Paren(inner: Expr) extends Expr
  final case class Literal(raw: Int) extends Expr
  final case class Ident(name: String) extends Expr

  def nodeCount(expr: Expr): Int = expr match {
    case Call(func, args) => nodeCount(func) + args.map(nodeCount).sum + 1
    case Index(t, i) => nodeCount(t) + nodeCount(i) + 1
    case Unary(_, arg) => nodeCount(arg) + 1
    case Binary(l, _, r) => nodeCount(l) + nodeCount(r) + 1
    case Paren(inner) => nodeCount(inner) + 1
    case Literal(_) | Ident(_) => 1
  }
}

object ExprTest extends App {
  import Expr._

  private val ten = Literal(10)
  private val add = Ident("add")
  println(nodeCount(Call(add, List(ten, ten))))
}