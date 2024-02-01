package exprparsing

import Models._
import Models.Expr._
import Models.ExprR._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.TermR._
import Models.Unary._
import org.scalacheck.Gen

object ExprGenerators {

  def exprGen: Gen[Expr] = {
    val lzyTermGen = Gen.lzy(termGen)
    val lzyExprRGen = Gen.lzy(exprRGen)
    Gen.zip(lzyTermGen, lzyExprRGen).map((Expr.apply _).tupled)
  }
  def exprRGen: Gen[ExprR] = {
    val lzyTermGen = Gen.lzy(termGen)
    val lzyExprRGen = Gen.lzy(exprRGen)
    val termExprRGen = Gen.zip(lzyTermGen, lzyExprRGen)
    Gen.frequency(
      5 -> Gen.const[ExprR](EEpsilon),
      1 -> termExprRGen.map[ExprR]((Add.apply _).tupled),
      1 -> termExprRGen.map[ExprR]((Sub.apply _).tupled)
    )
  }

  def termGen: Gen[Term] = {
    val lzyFactorGen = Gen.lzy(factorGen)
    val lzyTermRGen = Gen.lzy(termRGen)
    Gen.zip(lzyFactorGen, lzyTermRGen).map((Term.apply _).tupled)
  }
  def termRGen: Gen[TermR] = {
    val lzyFactorGen = Gen.lzy(factorGen)
    val lzyTermRGen = Gen.lzy(termRGen)
    val factorTermRGen = Gen.zip(lzyFactorGen, lzyTermRGen)
    Gen.frequency(
      4 -> Gen.const[TermR](TEpsilon),
      1 -> factorTermRGen.map[TermR]((Mul.apply _).tupled),
      1 -> factorTermRGen.map[TermR]((Div.apply _).tupled)
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
