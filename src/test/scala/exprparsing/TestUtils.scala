package exprparsing

import Models._
import Models.EvalError._
import munit.Assertions._
import munit.Location

object TestUtils {

  def assertEqualsEvalResultDouble(
    obtained: EvalResult[Double],
    expected: EvalResult[Double],
    delta: Double = 1e-12
  )(implicit loc: Location): Unit = (obtained, expected) match {
    case (Left(errObt), Left(errExp)) => assertEqualsEvalError(errObt, errExp, delta)
    case (Right(obt), Right(exp))     => assertEqualsDouble(obt, exp, delta)
    case _                            => fail(s"$obtained != $expected")
  }

  def assertEqualsEvalError(
    obtained: EvalError,
    expected: EvalError,
    delta: Double = 1e-12
  )(implicit loc: Location): Unit = (obtained, expected) match {
    case (DivisionByZero(nObt), DivisionByZero(nExp)) => assertEqualsDouble(nObt, nExp, delta)
    case (DivisionUndefined, DivisionUndefined)       => ()
    case (PowerWithNegativeBase(bObt, eObt), PowerWithNegativeBase(bExp, eExp)) =>
      assertEqualsDouble(bObt, bExp, delta)
      assertEqualsDouble(eObt, eExp, delta)
    case (PowerUndefined(bObt, eObt), PowerUndefined(bExp, eExp)) =>
      assertEqualsDouble(bObt, bExp, delta)
      assertEqualsDouble(eObt, eExp, delta)
    case _ => fail(s"obtained $obtained but expected $expected")
  }

}
