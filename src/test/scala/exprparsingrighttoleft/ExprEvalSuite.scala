package exprparsingrighttoleft

import ExprEval._
import ExprEvalSuite._
import ExprGenerators._
import Models._
import Models.EvalError._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Unary.Natural
import munit.Assertions._
import munit.{Location, ScalaCheckSuite}
import org.scalacheck.Prop._

class ExprEvalSuite extends ScalaCheckSuite {

  test("Add(1, 2.0) evaluates to Right(3.0)") {
    val expr = Add(Term.numb(1), Expr.numb(2.0))
    assertEqualsEitherEvalErrorDouble(eval(expr), 3.0)
  }

  test("Sub(1.0, 2) evaluates to Right(-1.0)") {
    val expr = Sub(Term.numb(1.0), Expr.numb(2))
    assertEqualsEitherEvalErrorDouble(eval(expr), -1.0)
  }

  test("Mul(3.0, -2) evaluates to Right(-6.0)") {
    val expr = Mul(Factor.numb(3.0), Term.numb(-2))
    assertEqualsEitherEvalErrorDouble(eval(expr), -6.0)
  }

  test("Div(9, 3.0) evaluates to Right(3.0)") {
    val expr = Div(Factor.numb(9), Term.numb(3.0))
    assertEqualsEitherEvalErrorDouble(eval(expr), 3.0)
  }

  test("Div(9, 0.0) evaluates to Left(DivisionByZero)") {
    val expr = Div(Factor.numb(9), Term.numb(0.0))
    assertEquals(eval(expr), Left(DivisionByZero))
  }

  test("Div(9, 0) evaluates to Left(DivisionByZero)") {
    val expr = Div(Factor.numb(9), Term.numb(0))
    assertEquals(eval(expr), Left(DivisionByZero))
  }

  test("Div(0, 0.0) evaluates to Left(DivisionUndefined)") {
    val expr = Div(Factor.numb(0), Term.numb(0.0))
    assertEquals(eval(expr), Left(DivisionUndefined))
  }

  test("Pow(2.0, 3) evaluates to Right(8.0)") {
    val expr = Pow(Power.numb(2.0), Factor.numb(3))
    assertEqualsEitherEvalErrorDouble(eval(expr), 8.0)
  }

  test("Pow(2.0, Infinity) evaluates to Right(Infinity)") {
    val expr = Pow(Power.numb(2.0), Factor.numb(Double.PositiveInfinity))
    assertEqualsEitherEvalErrorDouble(eval(expr), Double.PositiveInfinity)
  }

  test("Minus(1) evaluates to Right(-1.0)") {
    val expr = Minus(Natural(1))
    assertEqualsEitherEvalErrorDouble(eval(expr), -1.0)
  }

  property("Grouped(expr) and expr evaluate to same value") {
    forAll(exprGen) { expr =>
      val evaluatedGroupedExpr = eval(Expr.grouped(expr))
      eval(expr) match {
        case Right(d)    => assertEqualsEitherEvalErrorDouble(evaluatedGroupedExpr, d)
        case l @ Left(_) => assertEquals(evaluatedGroupedExpr, l)
      }
    }
  }

}
object ExprEvalSuite {

  def assertEqualsEitherEvalErrorDouble(
    obtained: Either[EvalError, Double],
    expected: Double,
    delta: Double = 1e-12
  )(implicit loc: Location): Unit = obtained match {
    case Left(err)  => fail(s"obtained Left($err) but expected Some($expected)")
    case Right(obt) => assertEqualsDouble(obt, expected, delta)
  }

}
