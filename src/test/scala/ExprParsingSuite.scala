import ExprParsing.Expr
import ExprParsing.Expr._
import ExprParsing.Term._
import ExprParsing.Factor._
import ExprParsing.Power._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop

class ExprParsingSuite extends ScalaCheckSuite {

  test("Add(1, 2.0) evals to Some(2.0)") {
    val expr = Add(tNumb(1), eNumb(2.0))
    assertEqualsDouble(expr.eval.get, 3.0, 1e-9)
  }

  test("Sub(1.0, 2) evals to Some(-1.0)") {
    val expr = Sub(tNumb(1.0), eNumb(2))
    assertEqualsDouble(expr.eval.get, -1.0, 1e-9)
  }

  test("Mul(3.0, -2) evals to Some(-6.0)") {
    val expr = Mul(fNumb(3.0), tNumb(-2))
    assertEqualsDouble(expr.eval.get, -6.0, 1e-9)
  }

  test("Div(9, 3.0) evals to Some(3.0)") {
    val expr = Div(fNumb(9), tNumb(3.0))
    assertEqualsDouble(expr.eval.get, 3.0, 1e-9)
  }

  test("Div(9, 0.0) evals to None") {
    val expr = Div(fNumb(9), tNumb(0.0))
    assertEquals(expr.eval, None)
  }

  test("Pow(2.0, 3) evals to Some(8.0)") {
    val expr = Pow(pNumb(2.0), fNumb(3))
    assertEqualsDouble(expr.eval.get, 8.0, 1e-9)
  }

}
object ExprParsingSuite {

  def exprGen: Gen[Expr] = ???

}
