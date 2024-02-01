package exprparsing
import ExprCodec._
import ExprCodecSuite._
import ExprEval._
import ExprGenerators._
import Models._
import Models.EvalError._
import Models.Expr._
import Models.ExprR._
import Models.Factor._
import Models.Term._
import Models.TermR._
import Models.Unary._
import munit.Assertions._
import munit.{Location, ScalaCheckSuite}
import org.scalacheck.Prop._

class ExprCodecSuite extends ScalaCheckSuite {

  /* parser */

  // TODO: ...

  /* encode */

  test("1+2.0 encoding is \"1+2.0\"") {
    val expr = Expr(Term.numb(1), Add(Term.numb(2.0), EEpsilon))
    assertEquals(encode(expr), "1+2.0")
  }

  test("1.0-2 encoding is \"1.0-2\"") {
    val expr = Expr(Term.numb(1.0), Sub(Term.numb(2), EEpsilon))
    assertEquals(encode(expr), "1.0-2")
  }

  test("1.0+(-2) encoding is \"(1.0)+(-2)\"") {
    val expr = Expr(Term.numb(1.0), Add(Term.numb(-2), EEpsilon))
    assertEquals(encode(expr), "(1.0)+(-2)")
  }

  test("3.0*2 encoding is \"3.0*2\"") {
    val expr = Expr(Term(Factor.numb(3.0), Mul(Factor.numb(2), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "3.0*2")
  }

  test("3.0*(-2) encoding is \"(3.0)*(-2)\"") {
    val expr = Expr(Term(Factor.numb(3.0), Mul(Factor.numb(-2), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "(3.0)*(-2)")
  }

  test("9/3.0 encoding is \"9/3.0\"") {
    val expr = Expr(Term(Factor.numb(9), Div(Factor.numb(3.0), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "9/3.0")
  }

  test("2.0^3 encoding is \"2.0^3\"") {
    val expr = Expr(Term(Pow(Power.numb(2.0), Factor.numb(3)), TEpsilon), EEpsilon)
    assertEquals(encode(expr), "2.0^3")
  }

  test("2^3^2 encoding is \"2^(3^2)\"") {
    val expr = Expr(Term(Pow(Power.numb(2), Pow(Power.numb(3), Factor.numb(2))), TEpsilon), EEpsilon)
    assertEquals(encode(expr), "2^(3^2)")
  }

  test("(2^3)^2 encoding is \"(2^3)^2\"") {
    val expr = Expr(
      Term(
        Pow(Power.grouped(Expr(Term(Pow(Power.numb(2), Factor.numb(3)), TEpsilon), EEpsilon)), Factor.numb(2)),
        TEpsilon
      ),
      EEpsilon
    )
    assertEquals(encode(expr), "(2^3)^2")
  }

  test("-1 encoding is \"-1\"") {
    val expr = Expr.numb(-1)
    assertEquals(encode(expr), "-1")
  }

  test("2.0*3+1.0 encoding is \"2.0*3+1.0\"") {
    val expr = Expr(Term(Factor.numb(2.0), Mul(Factor.numb(3), TEpsilon)), Add(Term.numb(1.0), EEpsilon))
    assertEquals(encode(expr), "2.0*3+1.0")
  }

  test("1.0+2-3.0*(1/5) encoding is \"1.0+2-(3.0*(1/5))\"") {
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
    assertEquals(encode(expr), "1.0+2-(3.0*(1/5))")
  }

  /* eval - parse - encode */

  // TODO: ...

}
object ExprCodecSuite {}
