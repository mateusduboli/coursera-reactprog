package calculator

import scala.util.DynamicVariable

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  var visited: DynamicVariable[Set[String]] = new DynamicVariable[Set[String]](Set())

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      (k, v) <- namedExpressions
    } yield (k, Signal({
      eval(v(), namedExpressions, Set(k))
    }))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], dependencies: Set[String]): Double = {
    expr match {
      case Literal(v) => v
      case Plus(a, b) => eval(a, references, dependencies) + eval(b, references, dependencies)
      case Minus(a, b) => eval(a, references, dependencies) - eval(b, references, dependencies)
      case Times(a, b) => eval(a, references, dependencies) * eval(b, references, dependencies)
      case Divide(a, b) => eval(a, references, dependencies) / eval(b, references, dependencies)
      case Ref(n) => {
        if(dependencies.contains(n)) Double.NaN
        else eval(getReferenceExpr(n, references), references, dependencies + n)
      }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
