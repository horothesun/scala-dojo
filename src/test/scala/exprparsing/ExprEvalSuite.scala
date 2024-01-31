package exprparsing

import ExprEval._
import ExprEvalSuite._
import ExprGenerators._
import Models._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Unary.Natural
import munit.Assertions._
import munit.{Location, ScalaCheckSuite}
import org.scalacheck.Prop._

class ExprEvalSuite extends ScalaCheckSuite {

  test("Add(1, 2.0) evaluates to Some(3.0)") {
    val expr = Add(Term.numb(1), Expr.numb(2.0))
    assertEqualsOptionDouble(eval(expr), 3.0)
  }

  test("Sub(1.0, 2) evaluates to Some(-1.0)") {
    val expr = Sub(Term.numb(1.0), Expr.numb(2))
    assertEqualsOptionDouble(eval(expr), -1.0)
  }

  test("Mul(3.0, -2) evaluates to Some(-6.0)") {
    val expr = Mul(Factor.numb(3.0), Term.numb(-2))
    assertEqualsOptionDouble(eval(expr), -6.0)
  }

  test("Div(9, 3.0) evaluates to Some(3.0)") {
    val expr = Div(Factor.numb(9), Term.numb(3.0))
    assertEqualsOptionDouble(eval(expr), 3.0)
  }

  test("Div(9, 0.0) evaluates to None") {
    val expr = Div(Factor.numb(9), Term.numb(0.0))
    assertEquals(eval(expr), None)
  }

  test("Div(9, 0) evaluates to None") {
    val expr = Div(Factor.numb(9), Term.numb(0))
    assertEquals(eval(expr), None)
  }

  test("Div(0, 0.0) evaluates to None") {
    val expr = Div(Factor.numb(0), Term.numb(0.0))
    assertEquals(eval(expr), None)
  }

  test("Pow(2.0, 3) evaluates to Some(8.0)") {
    val expr = Pow(Power.numb(2.0), Factor.numb(3))
    assertEqualsOptionDouble(eval(expr), 8.0)
  }

  test("Pow(2.0, Infinity) evaluates to Some(Infinity)") {
    val expr = Pow(Power.numb(2.0), Factor.numb(Double.PositiveInfinity))
    assertEqualsOptionDouble(eval(expr), Double.PositiveInfinity)
  }

  test("Minus(1) evaluates to Some(-1.0)") {
    val expr = Minus(Natural(1))
    assertEqualsOptionDouble(eval(expr), -1.0)
  }

  property("Grouped(expr) and expr evaluate to same value") {
    forAll(exprGen) { expr =>
      val evaluatedGroupedExpr = eval(Expr.grouped(expr))
      eval(expr) match {
        case Some(d) => assertEqualsOptionDouble(evaluatedGroupedExpr, d)
        case None    => assertEquals(evaluatedGroupedExpr, None)
      }
    }
  }

}
object ExprEvalSuite {

  def assertEqualsOptionDouble(
    obtained: Option[Double],
    expected: Double,
    delta: Double = 1e-12
  )(implicit loc: Location): Unit = obtained match {
    case None      => fail(s"obtained None but expected Some($expected)")
    case Some(obt) => assertEqualsDouble(obt, expected, delta)
  }

}
