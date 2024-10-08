package exprparsing

import ExprCodec._
import ExprCodecSuite._
import Models._
import Models.ExprR._
import Models.Factor._
import Models.TermR._
import Models.Unary._
import munit.{FunSuite, Location}
import munit.Assertions._

class ExprCodecSuite extends FunSuite {

  /* parser */

  test("\"1\" parses to Natural(1)") {
    assertFullyParsed(numericP.parse("1"), Natural(1))
  }

  test("\"1.0\" parses to NonNegDecimal(1.0)") {
    assertFullyParsed(numericP.parse("1.0"), NonNegDecimal(1.0))
  }

  test("Long.MaxValue parses to Natural(Long.MaxValue)") {
    assertFullyParsed(numericP.parse(s"${Long.MaxValue}"), Natural(Long.MaxValue))
  }

  test("\"1\" parses to Expr.numb(1)") {
    assertFullyParsed(exprP.parse("1"), Expr.numb(1))
  }

  test("\"1.0\" parses to Expr.numb(1.0)") {
    assertFullyParsed(exprP.parse("1.0"), Expr.numb(1.0))
  }

  test("\"1+2.0\" parses to Expr(1, Add(2.0, ε))") {
    assertFullyParsed(exprP.parse("1+2.0"), Expr(Term.numb(1), Add(Term.numb(2.0), EEpsilon)))
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

  test("Expr(1, Add(2.0, ε)) encoding is \"1+2.0\"") {
    val expr = Expr(Term.numb(1), Add(Term.numb(2.0), EEpsilon))
    assertEquals(encode(expr), "1+2.0")
  }

  test("Expr(1.0, Sub(2, ε)) encoding is \"1.0-2\"") {
    val expr = Expr(Term.numb(1.0), Sub(Term.numb(2), EEpsilon))
    assertEquals(encode(expr), "1.0-2")
  }

  test("Expr(Term(1.0), Add(Term(-2), ε)) encoding is \"(1.0)+(-2)\"") {
    // 1.0+(-2)
    val expr = Expr(Term.numb(1.0), Add(Term.numb(-2), EEpsilon))
    assertEquals(encode(expr), "(1.0)+(-2)")
  }

  test("Expr(Term(3.0, Mul(Factor(2), ε)), ε) encoding is \"3.0*2\"") {
    val expr = Expr(Term(Factor.numb(3.0), Mul(Factor.numb(2), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "3.0*2")
  }

  test("Expr(Term(Factor(3.0), Mul(Factor(-2), ε)), ε) encoding is \"(3.0)*(-2)\"") {
    // "3.0*(-2)"
    val expr = Expr(Term(Factor.numb(3.0), Mul(Factor.numb(-2), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "(3.0)*(-2)")
  }

  test("Expr(Term(Factor(9), Div(Factor(3.0), ε)), ε) encoding is \"9/3.0\"") {
    val expr = Expr(Term(Factor.numb(9), Div(Factor.numb(3.0), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "9/3.0")
  }

  test("Expr(Term(Factor(9), Div(Factor(0.0), ε)), ε) encoding is \"9/0.0\"") {
    val expr = Expr(Term(Factor.numb(9), Div(Factor.numb(0.0), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "9/0.0")
  }

  test("Expr(Term(Factor(9), Div(Factor(0), ε)), ε) encoding is \"9/0\"") {
    val expr = Expr(Term(Factor.numb(9), Div(Factor.numb(0), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "9/0")
  }

  test("Expr(Term(Factor(0), Div(Factor(0.0), ε)), ε) encoding is \"0/0.0\"") {
    val expr = Expr(Term(Factor.numb(0), Div(Factor.numb(0.0), TEpsilon)), EEpsilon)
    assertEquals(encode(expr), "0/0.0")
  }

  test("Expr(Term(Pow(Power(2.0), Factor(3)), ε), ε) encoding is \"2.0^3\"") {
    val expr = Expr(Term(Pow(Power.numb(2.0), Factor.numb(3)), TEpsilon), EEpsilon)
    assertEquals(encode(expr), "2.0^3")
  }

  test("Expr(Term(Pow(Power(2), Pow(Power(3), Factor(2))), ε), ε) encoding is \"2^(3^2)\"") {
    // 2^(3^2)
    val expr = Expr(Term(Pow(Power.numb(2), Pow(Power.numb(3), Factor.numb(2))), TEpsilon), EEpsilon)
    assertEquals(encode(expr), "2^(3^2)")
  }

  test(
    "Expr(Term(Pow(Power.grouped(Expr(Term(Pow(Power(2), Factor(3)), ε), ε)), Factor(2)), ε), ε)" +
      " encoding is \"(2^3)^2\""
  ) {
    // (2^3)^2
    val expr = Expr(
      Term(
        Pow(Power.grouped(Expr(Term(Pow(Power.numb(2), Factor.numb(3)), TEpsilon), EEpsilon)), Factor.numb(2)),
        TEpsilon
      ),
      EEpsilon
    )
    assertEquals(encode(expr), "(2^3)^2")
  }

  test("Expr(-1) encoding is \"-1\"") {
    val expr = Expr.numb(-1)
    assertEquals(encode(expr), "-1")
  }

  test("Expr(Term(Factor(2.0), Mul(Factor(3), ε)), Add(Term(1.0), ε)) encoding is \"2.0*3+1.0\"") {
    // "2.0*3+1.0"
    val expr = Expr(Term(Factor.numb(2.0), Mul(Factor.numb(3), TEpsilon)), Add(Term.numb(1.0), EEpsilon))
    assertEquals(encode(expr), "2.0*3+1.0")
  }

  test(
    "Expr(Term(1.0), Add(Term(2), Sub(Term(Factor(3.0), Mul(Factor.grouped(Expr(Term(Factor(1), Div(Factor(5), ε)), ε)), ε)), ε)))" +
      " encoding is \"1.0+2-(3.0*(1/5))\""
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
    assertEquals(encode(expr), "1.0+2-(3.0*(1/5))")
  }

}
object ExprCodecSuite {

  def assertFullyParsed[A](
    obtained: Either[cats.parse.Parser.Error, (String, A)],
    expected: A
  )(implicit loc: Location): Unit = obtained match {
    case Left(e)        => fail(s"parsing failed: $e")
    case Right(("", a)) => assertEquals(a, expected)
    case Right((s, a))  => fail(s"parser did not consume all input (remaining: \"$s\") and produced: $a")
  }

}
