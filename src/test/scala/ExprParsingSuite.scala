import ExprParsing._
import ExprParsing.Expr
import ExprParsing.Expr._
import ExprParsing.Term._
import ExprParsing.Factor._
import ExprParsing.Power._
import ExprParsing.Number._
import ExprParsingSuite._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ExprParsingSuite extends ScalaCheckSuite {

  test("Add(1, 2.0) evaluates to Some(2.0)") {
    val expr = Add(Term.numb(1), Expr.numb(2.0))
    assertEqualsDouble(eval(expr).get, 3.0, DOUBLE_EQ_DELTA)
  }

  test("Sub(1.0, 2) evaluates to Some(-1.0)") {
    val expr = Sub(Term.numb(1.0), Expr.numb(2))
    assertEqualsDouble(eval(expr).get, -1.0, DOUBLE_EQ_DELTA)
  }

  test("Mul(3.0, -2) evaluates to Some(-6.0)") {
    val expr = Mul(Factor.numb(3.0), Term.numb(-2))
    assertEqualsDouble(eval(expr).get, -6.0, DOUBLE_EQ_DELTA)
  }

  test("Div(9, 3.0) evaluates to Some(3.0)") {
    val expr = Div(Factor.numb(9), Term.numb(3.0))
    assertEqualsDouble(eval(expr).get, 3.0, DOUBLE_EQ_DELTA)
  }

  test("Div(9, 0.0) evaluates to None") {
    val expr = Div(Factor.numb(9), Term.numb(0.0))
    assertEquals(eval(expr), None)
  }

  test("Pow(2.0, 3) evaluates to Some(8.0)") {
    val expr = Pow(Power.numb(2.0), Factor.numb(3))
    assertEqualsDouble(eval(expr).get, 8.0, DOUBLE_EQ_DELTA)
  }

  property("bracketing an expression does not change its evaluation") {
    forAll(exprGen) { expr =>
      eval(expr) match {
        case Some(d) => assertEqualsDouble(eval(Brackets(expr)).get, d, DOUBLE_EQ_DELTA)
        case None    => assertEquals(eval(Brackets(expr)), None)
      }
    }
  }

  test("Add(1, Mul(2, 3.0)) encoding is \"1+2*3.0\"") {
    val expr = Add(
      Term.numb(1),
      ETerm(Mul(Factor.numb(2), Term.numb(3.0)))
    )
    assertEquals(encode(expr), "1+2*3.0")
  }

}
object ExprParsingSuite {

  val DOUBLE_EQ_DELTA: Double = 1e-9

  def exprGen: Gen[Expr] = {
    val lzyTermGen = Gen.lzy(termGen)
    val lzyExprGen = Gen.lzy(exprGen)
    Gen.frequency(
      1 -> Gen.zip(lzyTermGen, lzyExprGen).map((Add.apply _).tupled),
      1 -> Gen.zip(lzyTermGen, lzyExprGen).map((Sub.apply _).tupled),
      3 -> lzyTermGen.map(ETerm.apply)
    )
  }
  def termGen: Gen[Term] = {
    val lzyFactorGen = Gen.lzy(factorGen)
    val lzyTermGen = Gen.lzy(termGen)
    Gen.frequency(
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Mul.apply _).tupled),
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Div.apply _).tupled),
      3 -> lzyFactorGen.map(TFactor.apply)
    )
  }
  def factorGen: Gen[Factor] = {
    val lzyPowerGen = Gen.lzy(powerGen)
    val lzyFactorGen = Gen.lzy(factorGen)
    Gen.frequency(
      1 -> Gen.zip(lzyPowerGen, lzyFactorGen).map((Pow.apply _).tupled),
      3 -> lzyPowerGen.map(FPower.apply)
    )
  }
  def powerGen: Gen[Power] = {
    val lzyExprGen = Gen.lzy(exprGen)
    Gen.frequency(
      1 -> lzyExprGen.map(Brackets.apply),
      3 -> numberGen.map(PNumber.apply)
    )
  }
  def numberGen: Gen[Number] = Gen.oneOf(
    Gen.oneOf(Gen.negNum[Int], Gen.const(0), Gen.posNum[Int]).map(NInt.apply),
    Gen.oneOf(Gen.negNum[Double], Gen.const(0.0), Gen.posNum[Double]).map(NDecimal.apply)
  )

}
