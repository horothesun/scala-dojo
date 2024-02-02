package exprparsing

import ExprCodec._
import ExprCodecSuite._
import ExprEval._
import ExprGenerators._
import Models._
import Models.EvalError._
import Models.ExprR._
import Models.Factor._
import Models.TermR._
import Models.Unary._
import munit.Assertions._
import munit.{Location, ScalaCheckSuite}
import org.scalacheck.Prop._

class ExprCodecSuite extends ScalaCheckSuite {

  /* parser */

  test("\"1\" parses to Natural(1)") {
    assertFullyParsed(numericP.parse("1"), Natural(1))
  }

  test("\"1.0\" parses to NonNegDecimal(1.0)") {
    assertFullyParsed(numericP.parse("1.0"), NonNegDecimal(1.0))
  }

  test("\"1\" parses to Expr.numb(1)") {
    assertFullyParsed(exprP.parse("1"), Expr.numb(1))
  }

  test("\"1.0\" parses to Expr.numb(1.0)") {
    assertFullyParsed(exprP.parse("1.0"), Expr.numb(1.0))
  }

  test("\"1+2\" parses to Expr(1, Add(2, ε))") {
    assertFullyParsed(exprP.parse("1+2"), Expr(Term.numb(1), Add(Term.numb(2), EEpsilon)))
  }

  test("\"(42)\" parses to Grouped(Natural(42))") {
    assertFullyParsed(exprP.parse("(42)"), Expr.grouped(Expr.numb(42)))
  }

  test("\"(6/3)\" parses to Grouped(Div(6, 3))") {
    assertFullyParsed(
      exprP.parse("(6/3)"),
      Expr.grouped(Expr(Term(Factor.numb(6), Div(Factor.numb(3), TEpsilon)), EEpsilon))
    )
  }

  test("\"1+2*3\" parses to Expr(1, Add(Term(2, Mul(3, ε)), ε))") {
    assertFullyParsed(
      exprP.parse("1+2*3"),
      Expr(Term.numb(1), Add(Term(Factor.numb(2), Mul(Factor.numb(3), TEpsilon)), EEpsilon))
    )
  }

  test("\"1+6/3\" parses to Expr(1, Add(Term(6, Div(3, ε)), ε))") {
    assertFullyParsed(
      exprP.parse("1+6/3"),
      Expr(Term.numb(1), Add(Term(Factor.numb(6), Div(Factor.numb(3), TEpsilon)), EEpsilon))
    )
  }

  test("\"2^3^2\" parses to Expr(Term(Pow(2, Pow(3, 2)), ε), ε)") {
    assertFullyParsed(
      exprP.parse("2^3^2"),
      Expr(Term(Pow(Power.numb(2), Pow(Power.numb(3), Factor.numb(2))), TEpsilon), EEpsilon)
    )
  }

  test("\"2^3^4^5\" parses to Expr(Term(Pow(2, Pow(3, Pow(4, 5))), ε), ε)") {
    assertFullyParsed(
      exprP.parse("2^3^4^5"),
      Expr(Term(Pow(Power.numb(2), Pow(Power.numb(3), Pow(Power.numb(4), Factor.numb(5)))), TEpsilon), EEpsilon)
    )
  }

  test("\"1-2.0+3-4.0\" parses to Expr(1, Sub(2.0, Add(3, Sub(4.0, ε))))") {
    assertFullyParsed(
      exprP.parse("1-2.0+3-4.0"),
      Expr(Term.numb(1), Sub(Term.numb(2.0), Add(Term.numb(3), Sub(Term.numb(4.0), EEpsilon))))
    )
  }

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

  // It would be nicer to have an even stronger "parse(encode(expr)) == expr, with expr: Expr"
  property("eval(parse(encode(expr))) == eval(expr), with expr: Expr") {
    forAll(exprGen) { expr =>
      exprP.parse(encode(expr)) match {
        case Left(err)               => fail(s"parsing failed: $err")
        case Right(("", parsedExpr)) => assertEqualsEithersEvalErrorDouble(eval(parsedExpr), eval(expr))
        case Right((s, parsedExpr)) =>
          fail(s"parser did not consume all input (remaining: \"$s\") and produced: $parsedExpr")
      }
    }
  }

}
object ExprCodecSuite {

  def assertEqualsEithersEvalErrorDouble(
    obtained: Either[EvalError, Double],
    expected: Either[EvalError, Double],
    delta: Double = 1e-12
  )(implicit loc: Location): Unit = (obtained, expected) match {
    case (Left(errObt), Left(errExp)) if errObt == errExp => ()
    case (Right(obt), Right(exp))                         => assertEqualsDouble(obt, exp, delta)
    case _                                                => fail(s"$obtained != $expected")
  }

  def assertFullyParsed[A](
    obtained: Either[cats.parse.Parser.Error, (String, A)],
    expected: A
  )(implicit loc: Location): Unit = obtained match {
    case Left(e)        => fail(s"parsing failed: $e")
    case Right(("", a)) => assertEquals(a, expected)
    case Right((s, a))  => fail(s"parser did not consume all input (remaining: \"$s\") and produced: $a")
  }

}
