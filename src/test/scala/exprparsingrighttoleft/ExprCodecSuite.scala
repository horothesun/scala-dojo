package exprparsingrighttoleft

import ExprCodec._
import ExprCodecSuite._
import ExprEval._
import ExprGenerators._
import Models._
import Models.EvalError._
import Models.Expr._
import Models.Factor._
import Models.Term._
import Models.Unary._
import munit.{Location, ScalaCheckSuite}
import munit.Assertions._
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

  test("\"1+2\" parses to Add(1, 2)") {
    assertFullyParsed(exprP.parse("1+2"), Add(Term.numb(1), Expr.numb(2)))
  }

  test("\"(42)\" parses to Grouped(Natural(42))") {
    assertFullyParsed(exprP.parse("(42)"), Expr.grouped(Expr.numb(42)))
  }

  test("\"(6/3)\" parses to Grouped(Div(6, 3))") {
    assertFullyParsed(
      exprP.parse("(6/3)"),
      Expr.grouped(Expr.div(Factor.numb(6), Term.numb(3)))
    )
  }

  test("\"1+2*3\" parses to Add(1, Mul(2, 3))") {
    assertFullyParsed(
      exprP.parse("1+2*3"),
      Add(Term.numb(1), Expr.mul(Factor.numb(2), Term.numb(3)))
    )
  }

  test("\"1+6/3\" parses to Add(1, Div(6, 3))") {
    assertFullyParsed(
      exprP.parse("1+6/3"),
      Add(Term.numb(1), Expr.div(Factor.numb(6), Term.numb(3)))
    )
  }

  test("\"2^3^2\" parses to Pow(2, Pow(3, 2))") {
    assertFullyParsed(
      exprP.parse("2^3^2"),
      Expr.pow(Power.numb(2), Pow(Power.numb(3), Factor.numb(2)))
    )
  }

  test("\"2^3^4^5\" parses to Pow(2, Pow(3, Pow(4, 5)))") {
    assertFullyParsed(
      exprP.parse("2^3^4^5"),
      Expr.pow(Power.numb(2), Pow(Power.numb(3), Pow(Power.numb(4), Factor.numb(5))))
    )
  }

  // TODO: fix associativity!!!
//  test("\"1-2.0+3-4.0\" parses to Sub(Add(Sub(1, 2.0), 3), 4.0)") {
  test("\"1-2.0+3-4.0\" parses to Sub(1, Add(2.0, Sub(3, 4.0)))") {
    assertFullyParsed(
      exprP.parse("1-2.0+3-4.0"),
//      Sub(Term.expr(Add(Term.expr(Sub(Term.numb(1), Expr.numb(2.0))), Expr.numb(3))), Expr.numb(4.0))
      Sub(Term.numb(1), Add(Term.numb(2.0), Sub(Term.numb(3), Expr.numb(4.0))))
    )
  }

  /* encode */

  test("Add(1, Mul(2, 3.0)) encoding is \"1+(2*3.0)\"") {
    val expr = Add(
      Term.numb(1),
      Expr.mul(Factor.numb(2), Term.numb(3.0))
    )
    assertEquals(encode(expr), "1+(2*3.0)")
  }

  test("Add(Mul(2, 3.0), 1) encoding is \"(2*3.0)+1\"") {
    val expr = Add(
      Mul(Factor.numb(2), Term.numb(3.0)),
      Expr.numb(1)
    )
    assertEquals(encode(expr), "(2*3.0)+1")
  }

  test("Add(Mul(2, 3.0), Minus(1)) encoding is \"(2*3.0)+(-1)\"") {
    val expr = Add(
      Mul(Factor.numb(2), Term.numb(3.0)),
      Expr.numb(-1)
    )
    assertEquals(encode(expr), "(2*3.0)+(-1)")
  }

  test("Mul(2.0, Div(Sub(3.0, 1), 5)) encoding is \"2.0*((3.0-1)/5)\"") {
    val expr = Mul(
      Factor.numb(2.0),
      Div(
        Factor.expr(Sub(Term.numb(3.0), Expr.numb(1))),
        Term.numb(5)
      )
    )
    assertEquals(encode(expr), "2.0*((3.0-1)/5)")
  }

  test("Add(1.5, Minus(Sub(Minus(2.5), 3))) encoding is \"1.5+(-((-(2.5))-3))\"") {
    val expr = Add(
      Term.numb(1.5),
      Expr.neg(
        Sub(Term.neg(Expr.numb(2.5)), Expr.numb(3))
      )
    )
    assertEquals(encode(expr), "1.5+(-((-(2.5))-3))")
  }

  /* eval - parse - encode */

  // It would be nicer to have an even stronger "parse(encode(expr)) == expr, with expr: Expr"
  property("eval(parse(encode(expr))) == eval(expr), with expr: Expr") {
    forAll(exprGen) { expr =>
      exprP.parse(encode(expr)) match {
        case Left(err)               => fail(s"parsing failed: $err")
        case Right(("", parsedExpr)) => assertEqualsEithersEvalErrorDouble(eval(parsedExpr), eval(expr))
        case Right((s, parsedExpr))  =>
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
    case (Left(DivisionByZero), Left(DivisionByZero)) | (Left(DivisionUndefined), Left(DivisionUndefined)) |
        (Left(PowerWithNegativeBase), Left(PowerWithNegativeBase)) | (Left(PowerUndefined), Left(PowerUndefined)) =>
      ()
    case (Right(obt), Right(exp)) => assertEqualsDouble(obt, exp, delta)
    case _                        => fail(s"$obtained != $expected")
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
