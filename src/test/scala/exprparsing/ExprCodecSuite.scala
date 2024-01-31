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
    val expr = Plus(Power.numb(1))
    assertEqualsOptionDouble(eval(expr), 1.0)
  }

  test("Minus(1) evaluates to Some(-1.0)") {
    val expr = Minus(Power.numb(1))
    assertEqualsOptionDouble(eval(expr), -1.0)
  }

//  property("Grouped(expr) and expr evaluate to same value") {
//    forAll(exprGen) { expr =>
//      val evaluatedGroupedExpr = eval(Grouped(expr))
//      eval(expr) match {
//        case Some(d) => assertEqualsOptionDouble(evaluatedGroupedExpr, d)
//        case None    => assertEquals(evaluatedGroupedExpr, None)
//      }
//    }
//  }

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

  test("\"1\" parses to Natural(1)") {
    assertFullyParsed(numericP.parse("1"), Natural(1))
  }

  test("\"1.0\" parses to NonNegDecimal(1.0)") {
    assertFullyParsed(numericP.parse("1.0"), NonNegDecimal(1.0))
  }

//  test("\"1\" parse to Expr.numb(1)") {
//    assertFullyParsed(exprP.parse("1"), Expr.numb(1))
//  }

//  test("\"1+2\" parse to Add(1, 2)") {
//    assertFullyParsed(exprP.parse("1+2"), Add(Term.numb(1), Expr.numb(2)))
//  }

//  test("\"1+2*3\" parses to Add(1, Mul(2, 3))") {
//    assertFullyParsed(
//      exprP.parse("1+2*3"),
//      Add(Term.numb(1), Expr.mul(Factor.numb(2), Term.numb(3)))
//    )
//  }

//  test("\"1+(6/3)\" parses to Add(1, Div(6, 3))") {
//    assertFullyParsed(
//      exprP.parse("1+(6/3)"),
//      Add(Term.numb(1), Expr.div(Factor.numb(6), Term.numb(3)))
//    )
//  }

//  test("\"(6/3)\" parses to Div(6, 3)") {
//    assertFullyParsed(
//      exprP.parse("(6/3)"),
//      Expr.div(Factor.numb(6), Term.numb(3))
//    )
//  }

//  test("\"(42)\" parses to Natural(42)") {
//    assertFullyParsed(exprP.parse("(42)"), Expr.numb(42))
//  }

  test("parse and eval few valid expressions") {
    validExpressionsAndResults.map { case (e, res) =>
      val parsedAndEvaluated = exprP.parse(e).map { case (s, expr) => (s, eval(expr)) }
      assertFullyParsed(parsedAndEvaluated, res)
    }
  }

}
object ExprCodecSuite {

  val validExpressionsAndResults: List[(String, Option[Double])] = List(
//    ("(3.14159+6.28318)/2.71828", Some(3)),
//    ("48*(5-2)^3", Some(1296)),
//    ("(84.123-9.372)/0.75", Some(99)),
//    ("5.678*(-3.14159+2.71828)", Some(-2.40355)),
//    ("(7.345+9.654)/(0.123-0.045)", Some(217)),
//    ("8*(-0.5^2)-1.234", Some(.366)),
//    ("43.567/(12.89+3.14)", Some(2)),
//    ("(7.345+9.654)/(0.123-0.045)", Some(217)),
//    ("-5.123*(-2^3)+3.789", Some(44.773)),
//    ("(100/5)^3-21.678", Some(7978.322)),
//    ("8.765*(3^2-2)-4.321", Some(57.034)),
//    ("(5+7)^2+5.342", Some(149.342)),
//    ("15.987/(3.14-2.718)", Some(37)),
//    ("-7.531*(-8^2+1)+9.012", Some(-480.503)),
//    ("(200/4)^3+8.743", Some(125008.743)),
//    ("6.423*(5^2+3)-1.987", Some(177.857)),
//    ("(9+11)^2-12.567", Some(387.433)),
//    ("24.098/(5.314-1.234)", Some(5)),
//    ("-9.765*(-4^3)+2.109", Some(627.069)),
//    ("(300/6)^3-34.897", Some(124965.103)),
//    ("4.123*(7^2-8)-5.678", Some(163.365)),
//    ("(13+17)^2+17.345", Some(917.345)),
//    ("33.210/(7.531-4.210)", Some(10)),
//    ("-8.456*(-2^2+2)+6.234", Some(-44.502)),
//    ("(400/8)^3+51.023", Some(125051.023)),
//    ("2.718*(9^2+1)-8.965", Some(213.911)),
//    ("(19+23)^2-24.123", Some(1739.877)),
//    ("42.345/(9.765-6.109)", Some(11)),
//    ("-7.123*(-3^3)+4.356", Some(196.677)),
//    ("(500/10)^3+72.159", Some(125072.159))
  )

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
      20 -> lzyTermGen.map(ETerm.apply)
    )
  }
  def termGen: Gen[Term] = {
    val lzyFactorGen = Gen.lzy(factorGen)
    val lzyTermGen = Gen.lzy(termGen)
    Gen.frequency(
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Mul.apply _).tupled),
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Div.apply _).tupled),
      20 -> lzyFactorGen.map(TFactor.apply)
    )
  }
  def factorGen: Gen[Factor] = {
    val lzyPowerGen = Gen.lzy(powerGen)
    val lzyFactorGen = Gen.lzy(factorGen)
    Gen.frequency(
      1 -> Gen.zip(lzyPowerGen, lzyFactorGen).map((Pow.apply _).tupled),
      20 -> lzyPowerGen.map(FPower.apply)
    )
  }
  def powerGen: Gen[Power] = {
    val lzyPowerGen = Gen.lzy(powerGen)
    val lzyUnaryGen = Gen.lzy(unaryGen)
    Gen.frequency(
      1 -> lzyPowerGen.map(Plus.apply),
      1 -> lzyPowerGen.map(Minus.apply),
      20 -> lzyUnaryGen.map(PUnary.apply)
    )
  }
  def unaryGen: Gen[Unary] = Gen.frequency(
    20 -> naturalGen,
    20 -> nonNegDecimalGen,
    1 -> Gen.lzy(groupedGen)
  )
  def naturalGen: Gen[Natural] = Gen.oneOf(Gen.const(0), Gen.posNum[Int]).map(Natural.apply)
  def nonNegDecimalGen: Gen[NonNegDecimal] = Gen.oneOf(Gen.const(0.0), Gen.posNum[Double]).map(NonNegDecimal.apply)
  def groupedGen: Gen[Grouped] = {
    val lzyExprGen = Gen.lzy(exprGen)
    lzyExprGen.map(Grouped.apply)
  }

}
