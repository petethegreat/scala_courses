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
    namedExpressions.transform( (k,v) => Var(eval(v(),namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name,references),references - name)
        // something like this??? if a reference has been looked up, it is no longer known. This will prevent circular stuff.//
        // our reference could be "b"
        // b could contain the expression "c + 1"
        // b could contain the (circular) expression "b + 1"
        // need to work back through the references until we get to a literal.
      case Plus(a,b) => eval(a,references) + eval(b,references)
        // 2.0 + c
      case Minus(a,b) => eval(a,references) - eval(b,references)
      case Divide(a,b) => eval(a,references) / eval(b,references)
      case Times(a,b) => eval(a,references) * eval(b,references)
      case _ => 0.0
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
      exprSignal()
    }
  }
}
