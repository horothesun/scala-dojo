package exprparsing

import Models._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Unary._
import ExprCodec._
import ExprCodecSuite._
import ExprEval._
import munit.Assertions._
import munit.{Location, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ExprCodecSuite extends ScalaCheckSuite {

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

  test("Plus(1) evaluates to Some(1.0)") {
    val expr = Plus(Expr.numb(1))
    assertEqualsOptionDouble(eval(expr), 1.0)
  }

  test("Minus(1) evaluates to Some(-1.0)") {
    val expr = Minus(Expr.numb(1))
    assertEqualsOptionDouble(eval(expr), -1.0)
  }

  property("Plus(expr) and expr evaluate to same value") {
    forAll(exprGen) { expr =>
      val evaluatedPlusExpr = eval(Plus(expr))
      eval(expr) match {
        case Some(d) => assertEqualsOptionDouble(evaluatedPlusExpr, d)
        case None    => assertEquals(evaluatedPlusExpr, None)
      }
    }
  }

  property("Natural(i) throws exception when i < 0") {
    forAll(Gen.negNum[Int]) { i =>
      intercept[java.lang.IllegalArgumentException](Natural(i))
      ()
    }
  }

  property("Natural(i) correctly created when i >= 0") {
    forAll(Gen.oneOf(Gen.const(0), Gen.posNum[Int])) { i =>
      assert(Natural(i).i >= 0)
    }
  }

  property("NonNegDecimal(d) throws exception when d < 0") {
    forAll(Gen.negNum[Double]) { d =>
      intercept[java.lang.IllegalArgumentException](NonNegDecimal(d))
      ()
    }
  }

  property("NonNegDecimal(d) correctly created when d >= 0.0") {
    forAll(Gen.oneOf(Gen.const(0.0), Gen.posNum[Double])) { d =>
      assert(NonNegDecimal(d).d >= 0.0)
    }
  }

  test("Add(1, Mul(2, 3.0)) encoding is \"1+(2*3.0)\"") {
    val expr = Add(
      Term.numb(1),
      ETerm(Mul(Factor.numb(2), Term.numb(3.0)))
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

  test("Add(1.5, Minus(Sub(Minus(2.5), 3))) encoding is \"1.5+(-((-2.5)-3))\"") {
    val expr = Add(
      Term.numb(1.5),
      Expr.neg(
        Sub(Term.neg(Expr.numb(2.5)), Expr.numb(3))
      )
    )
    assertEquals(encode(expr), "1.5+(-((-2.5)-3))")
  }

  test("\"1\" parses to Unary.Natural(1)") {
    assertFullyParsed(unaryP.parse("1"), Natural(1))
  }

  test("\"1.0\" parses to Unary.NonNegDecimal(1.0)") {
    assertFullyParsed(unaryP.parse("1.0"), NonNegDecimal(1.0))
  }

  test("\"1\" parse to Expr.numb(1)") {
    assertFullyParsed(exprP.parse("1"), Expr.numb(1))
  }

  test("\"1+2\" parse to Add(1, 2)") {
    assertFullyParsed(exprP.parse("1+2"), Add(Term.numb(1), Expr.numb(2)))
  }

  test("\"1+2*3\" parses to Add(1, Mul(2, 3))") {
    assertFullyParsed(
      exprP.parse("1+2*3"),
      Add(Term.numb(1), Expr.mul(Factor.numb(2), Term.numb(3)))
    )
  }

}
object ExprCodecSuite {

  def assertEqualsOptionDouble(
    obtained: Option[Double],
    expected: Double,
    delta: Double = 1e-12
  )(implicit loc: Location): Unit = obtained match {
    case None    => fail(s"obtained None but expected Some($expected)")
    case Some(o) => assertEqualsDouble(o, expected, delta)
  }

  def assertFullyParsed[A](
    obtained: Either[cats.parse.Parser.Error, (String, A)],
    expected: A
  )(implicit loc: Location): Unit = obtained match {
    case Left(e)        => fail(s"parsing failed: $e")
    case Right(("", a)) => assertEquals(a, expected)
    case Right((s, a))  => fail(s"parser did not consume all input (remaining: \"$s\") and produced: $a")
  }

  def exprGen: Gen[Expr] = {
    val lzyTermGen = Gen.lzy(termGen)
    val lzyExprGen = Gen.lzy(exprGen)
    Gen.frequency(
      1 -> Gen.zip(lzyTermGen, lzyExprGen).map((Add.apply _).tupled),
      1 -> Gen.zip(lzyTermGen, lzyExprGen).map((Sub.apply _).tupled),
      5 -> lzyTermGen.map(ETerm.apply)
    )
  }
  def termGen: Gen[Term] = {
    val lzyFactorGen = Gen.lzy(factorGen)
    val lzyTermGen = Gen.lzy(termGen)
    Gen.frequency(
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Mul.apply _).tupled),
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Div.apply _).tupled),
      5 -> lzyFactorGen.map(TFactor.apply)
    )
  }
  def factorGen: Gen[Factor] = {
    val lzyPowerGen = Gen.lzy(powerGen)
    val lzyFactorGen = Gen.lzy(factorGen)
    Gen.frequency(
      1 -> Gen.zip(lzyPowerGen, lzyFactorGen).map((Pow.apply _).tupled),
      5 -> lzyPowerGen.map(FPower.apply)
    )
  }
  def powerGen: Gen[Power] = {
    val lzyExprGen = Gen.lzy(exprGen)
    val lzyUnaryGen = Gen.lzy(unaryGen)
    Gen.frequency(
      1 -> lzyExprGen.map(Plus.apply),
      1 -> lzyExprGen.map(Minus.apply),
      5 -> lzyUnaryGen.map(PUnary.apply)
    )
  }
  def unaryGen: Gen[Unary] = Gen.oneOf(naturalGen, nonNegDecimalGen)
  def naturalGen: Gen[Natural] = Gen.oneOf(Gen.const(0), Gen.posNum[Int]).map(Natural.apply)
  def nonNegDecimalGen: Gen[NonNegDecimal] = Gen.oneOf(Gen.const(0.0), Gen.posNum[Double]).map(NonNegDecimal.apply)

}