package exprparsing

import ExprEval._
import ExprEvalSuite._
import ExprGenerators._
import Models._
import Models.ExprR._
import Models.TermR._
import Models.Factor._
import Models.EvalError._
import munit.Assertions._
import munit.{Location, ScalaCheckSuite}
import org.scalacheck.Prop._

class ExprEvalSuite extends ScalaCheckSuite {

  test("Expr(1, Add(2.0, ε)) evaluates to Right(3.0)") {
    val expr = Expr(Term.numb(1), Add(Term.numb(2.0), EEpsilon))
    assertEqualsEvalResultDouble(eval(expr), 3.0)
  }

  test("Expr(1.0, Sub(2, ε)) evaluates to Right(-1.0)") {
    val expr = Expr(Term.numb(1.0), Sub(Term.numb(2), EEpsilon))
    assertEqualsEvalResultDouble(eval(expr), -1.0)
  }

  test("Expr(Term(1.0), Add(Term(-2), ε)) evaluates to Right(-1.0)") {
    // 1.0+(-2)
    val expr = Expr(Term.numb(1.0), Add(Term.numb(-2), EEpsilon))
    assertEqualsEvalResultDouble(eval(expr), -1.0)
  }

  test("Expr(Term(3.0, Mul(Factor(2), ε)), ε) evaluates to Right(6.0)") {
    val expr = Expr(Term(Factor.numb(3.0), Mul(Factor.numb(2), TEpsilon)), EEpsilon)
    assertEqualsEvalResultDouble(eval(expr), 6.0)
  }

  test("Expr(Term(Factor(3.0), Mul(Factor(-2), ε)), ε) evaluates to Right(-6.0)") {
    // "3.0*(-2)"
    val expr = Expr(Term(Factor.numb(3.0), Mul(Factor.numb(-2), TEpsilon)), EEpsilon)
    assertEqualsEvalResultDouble(eval(expr), -6.0)
  }

  test("Expr(Term(Factor(9), Div(Factor(3.0), ε)), ε) evaluates to Right(3.0)") {
    val expr = Expr(Term(Factor.numb(9), Div(Factor.numb(3.0), TEpsilon)), EEpsilon)
    assertEqualsEvalResultDouble(eval(expr), 3.0)
  }

  test("Expr(Term(Factor(9), Div(Factor(0.0), ε)), ε) evaluates to Left(DivisionByZero)") {
    val expr = Expr(Term(Factor.numb(9), Div(Factor.numb(0.0), TEpsilon)), EEpsilon)
    assertEquals(eval(expr), Left(DivisionByZero))
  }

  test("Expr(Term(Factor(9), Div(Factor(0), ε)), ε) evaluates to Left(DivisionByZero)") {
    val expr = Expr(Term(Factor.numb(9), Div(Factor.numb(0), TEpsilon)), EEpsilon)
    assertEquals(eval(expr), Left(DivisionByZero))
  }

  test("Expr(Term(Factor(0), Div(Factor(0.0), ε)), ε) evaluates to Left(DivisionUndefined)") {
    val expr = Expr(Term(Factor.numb(0), Div(Factor.numb(0.0), TEpsilon)), EEpsilon)
    assertEquals(eval(expr), Left(DivisionUndefined))
  }

  test("Expr(Term(Pow(Power(2.0), Factor(3)), ε), ε) evaluates to Right(8.0)") {
    val expr = Expr(Term(Pow(Power.numb(2.0), Factor.numb(3)), TEpsilon), EEpsilon)
    assertEqualsEvalResultDouble(eval(expr), 8.0)
  }

  test("Expr(Term(Pow(Power(2.0), Factor(Double.Infinity)), ε), ε) evaluates to Right(Infinity)") {
    val expr = Expr(Term(Pow(Power.numb(2.0), Factor.numb(Double.PositiveInfinity)), TEpsilon), EEpsilon)
    assertEqualsEvalResultDouble(eval(expr), Double.PositiveInfinity)
  }

  test("Expr(Term(Pow(Power(2), Pow(Power(3), Factor(2))), ε), ε) evaluates to Right(512.0)") {
    // 2^(3^2)
    val expr = Expr(Term(Pow(Power.numb(2), Pow(Power.numb(3), Factor.numb(2))), TEpsilon), EEpsilon)
    assertEqualsEvalResultDouble(eval(expr), 512.0)
  }

  test(
    "Expr(Term(Pow(Power.grouped(Expr(Term(Pow(Power(2), Factor(3)), ε), ε)), Factor(2)), ε), ε)" +
      " evaluates to Right(64.0)"
  ) {
    // (2^3)^2
    val expr = Expr(
      Term(
        Pow(Power.grouped(Expr(Term(Pow(Power.numb(2), Factor.numb(3)), TEpsilon), EEpsilon)), Factor.numb(2)),
        TEpsilon
      ),
      EEpsilon
    )
    assertEqualsEvalResultDouble(eval(expr), 64.0)
  }

  test("Expr(-1) evaluates to Right(-1.0)") {
    val expr = Expr.numb(-1)
    assertEqualsEvalResultDouble(eval(expr), -1.0)
  }

  test("Expr(Term(Factor(2.0), Mul(Factor(3), ε)), Add(Term(1.0), ε)) evaluates to Right(7.0)") {
    // "2.0*3+1.0"
    val expr = Expr(Term(Factor.numb(2.0), Mul(Factor.numb(3), TEpsilon)), Add(Term.numb(1.0), EEpsilon))
    assertEqualsEvalResultDouble(eval(expr), 7.0)
  }

  test(
    "Expr(Term(1.0), Add(Term(2), Sub(Term(Factor(3.0), Mul(Factor.grouped(Expr(Term(Factor(1), Div(Factor(5), ε)), ε)), ε)), ε)))" +
      " evaluates to Right(2.4)"
  ) {
    // "1.0+2-3.0*(1/5)"
    val expr = Expr(
      Term.numb(1.0),
      Add(
        Term.numb(2),
        Sub(
          Term(
            Factor.numb(3.0),
            Mul(Factor.grouped(Expr(Term(Factor.numb(1), Div(Factor.numb(5), TEpsilon)), EEpsilon)), TEpsilon)
          ),
          EEpsilon
        )
      )
    )
    assertEqualsEvalResultDouble(eval(expr), 2.4)
  }

  property("Grouped(expr) and expr evaluate to same value") {
    forAll(exprGen) { expr =>
      val evaluatedGroupedExpr = eval(Expr.grouped(expr))
      eval(expr) match {
        case Right(d)    => assertEqualsEvalResultDouble(evaluatedGroupedExpr, d)
        case l @ Left(_) => assertEquals(evaluatedGroupedExpr, l)
      }
    }
  }

}
object ExprEvalSuite {

  def assertEqualsEvalResultDouble(
    obtained: EvalResult[Double],
    expected: Double,
    delta: Double = 1e-12
  )(implicit loc: Location): Unit = obtained match {
    case Left(err)  => fail(s"obtained Left($err) but expected Right($expected)")
    case Right(obt) => assertEqualsDouble(obt, expected, delta)
  }

}
