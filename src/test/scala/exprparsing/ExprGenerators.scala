package exprparsing

import Models._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Unary._
import org.scalacheck.Gen

object ExprGenerators {

  def exprGen: Gen[Expr] = {
    val lzyTermGen = Gen.lzy(termGen)
    val lzyExprGen = Gen.lzy(exprGen)
    Gen.frequency(
      1 -> Gen.zip(lzyTermGen, lzyExprGen).map((Add.apply _).tupled),
      1 -> Gen.zip(lzyTermGen, lzyExprGen).map((Sub.apply _).tupled),
      4 -> lzyTermGen.map(ETerm.apply)
    )
  }
  def termGen: Gen[Term] = {
    val lzyFactorGen = Gen.lzy(factorGen)
    val lzyTermGen = Gen.lzy(termGen)
    Gen.frequency(
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Mul.apply _).tupled),
      1 -> Gen.zip(lzyFactorGen, lzyTermGen).map((Div.apply _).tupled),
      4 -> lzyFactorGen.map(TFactor.apply)
    )
  }
  def factorGen: Gen[Factor] = {
    val lzyPowerGen = Gen.lzy(powerGen)
    val lzyFactorGen = Gen.lzy(factorGen)
    Gen.frequency(
      1 -> Gen.zip(lzyPowerGen, lzyFactorGen).map((Pow.apply _).tupled),
      4 -> lzyPowerGen.map(FPower.apply)
    )
  }
  def powerGen: Gen[Power] = {
    val lzyUnaryGen = Gen.lzy(unaryGen)
    Gen.frequency(
      1 -> lzyUnaryGen.map(Minus.apply),
      4 -> lzyUnaryGen.map(PUnary.apply)
    )
  }
  def unaryGen: Gen[Unary] = Gen.frequency(
    2 -> naturalGen,
    2 -> nonNegDecimalGen,
    1 -> Gen.lzy(groupedGen)
  )
  def naturalGen: Gen[Natural] = Gen.oneOf(Gen.const(0), Gen.posNum[Int]).map(Natural.apply)
  def nonNegDecimalGen: Gen[NonNegDecimal] = Gen.oneOf(Gen.const(0.0), Gen.posNum[Double]).map(NonNegDecimal.apply)
  def groupedGen: Gen[Grouped] = {
    val lzyExprGen = Gen.lzy(exprGen)
    lzyExprGen.map(Grouped.apply)
  }

}
