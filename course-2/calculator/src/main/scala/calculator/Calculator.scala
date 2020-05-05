package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      namedExpressions.map(entry =>
        (entry._1, Signal(eval(getReferenceExpr(entry._1, namedExpressions), namedExpressions - entry._1))))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(x) => x
      case Ref(name) => eval(getReferenceExpr(name, references), references - name)
      case Plus(x, y) => eval(x, references) + eval(y, references)
      case Minus(x, y) => eval(x, references) - eval(y, references)
      case Times(x, y) => eval(x, references) * eval(y, references)
      case Divide(x, y) => eval(x, references) / eval(y, references)
      case _ => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal() match {
        case Ref(x) if x == name => Literal(Double.NaN)
        case _ => exprSignal()
      }
    }
  }
}
