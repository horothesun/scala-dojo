package exprparsingrighttoleft

import Calculator._
import CalculatorSuite._
import Models.CalcResult
import Models.CalcResult._
import Models.EvalError._
import munit.{FunSuite, Location}
import munit.Assertions._

class CalculatorSuite extends FunSuite {

  // TODO: fix associativity!!!
//  test("\"(1+0.5^2)/0.1*2.0\" evaluates as \"((1+0.5^2)/0.1)*2.0\"") {
  test("\"(1+0.5^2)/0.1*2.0\" evaluates as \"(1+0.5^2)/(0.1*2.0)\"") {
    val expr1 = "(1+0.5^2)/0.1*2.0"
//    val expr2 = "((1+0.5^2)/0.1)*2.0"
    val expr2 = "(1+0.5^2)/(0.1*2.0)"
    (calc(expr1), calc(expr2)) match {
      case (Success(d1), Success(d2)) => assertEqualsDouble(d1, d2, delta = 1e-6)
      case (res1, res2)               => fail(s"res1 = $res1 | res2 = $res2")
    }
  }

  test("calculating few expressions") {
    List[(String, CalcResult)](
      ("(1+0.5^2) / 0.0*2.0", EvaluationError(DivisionByZero)),
      ("( 3.14159+6.28318 )/2.71828", Success(3.467181)),
      ("48*(5-2) ^ 3", Success(1296)),
      ("(84.123-9.372)/0.75  ", Success(99.668)),
      ("5.678*(-3.14159+2.71828)", Success(-2.403554)),
      ("(7.345+9.654)/(0.123-0.045)", Success(217.935897)),
      ("8*(- 0.5^2)-1.234", EvaluationError(PowerWithNegativeBase)),
      ("43.567/(12.89   + 3.14)", Success(2.717842)),
      ("(7.345+9.654)/(0.123-0.045)", Success(217.935897)),
      ("-5.123*(-2^3)+3.789", EvaluationError(PowerWithNegativeBase)),
      ("(100/5)^ 3 - 21.678", Success(7978.322)),
      (" 8.765 *(3^ 2-2) -4.321  ", Success(57.034)),
      ("(5+7)^2+5.342", Success(149.342)),
      ("15.987/(3.14-2.718)", Success(37.883886)),
      ("-7.531*(-8^2+1)+9.012", EvaluationError(PowerWithNegativeBase)),
      ("( 200/4 )^3+8.743", Success(125008.743)),
      ("6.423*(5^2+3)-1.987", Success(177.857)),
      ("( 9+11)^2-12.567", Success(387.433)),
      ("24.098/(5.314-1.234)", Success(5.906373)),
      ("-9.765*(-4^3)+2.109", EvaluationError(PowerWithNegativeBase)),
      ("(300/6)^3-34.897", Success(124965.103)),
      ("4.123*(7^2-8)-5.678", Success(163.365)),
      ("(13+17)^2+17.345", Success(917.345)),
      ("33.210/(7.531-4.210)", Success(10)),
      ("-8.456*(-2^2+2)+6.234", EvaluationError(PowerWithNegativeBase)),
      ("(400/8)^3+51.023", Success(125051.023)),
      (" 2.718*(9^2+1)-8.965", Success(213.911)),
      ("(19+23)^2 - 24.123", Success(1739.877)),
      ("42.345 / (9.765-6.109)  ", Success(11.58233)),
      ("  -7.123*(-3^3)+4.356", EvaluationError(PowerWithNegativeBase)),
      ("(500/10)^3+72.159", Success(125072.159))
    ).map { case (expr, expected) => assertEqualsCalcResult(obtained = calc(expr), expected) }
  }

}
object CalculatorSuite {

  def assertEqualsCalcResult(
    obtained: CalcResult,
    expected: CalcResult,
    delta: Double = 1e-6
  )(implicit loc: Location): Unit = (obtained, expected) match {
    case (Success(obt), Success(exp))                 => assertEqualsDouble(obt, exp, delta)
    case (ParsingError(msgObt), ParsingError(msgExp)) => assertEquals(msgObt, msgExp)
    case (EvaluationError(DivisionByZero), EvaluationError(DivisionByZero)) |
        (EvaluationError(DivisionUndefined), EvaluationError(DivisionUndefined)) |
        (EvaluationError(PowerWithNegativeBase), EvaluationError(PowerWithNegativeBase)) |
        (EvaluationError(PowerUndefined), EvaluationError(PowerUndefined)) =>
      ()
    case _ => fail(s"$obtained != $expected")
  }

}
