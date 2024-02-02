package exprparsing

import Models.ExprR._
import Models.Power._
import Models.TermR._
import Models.Token._
import Models.Unary._

object Models {

  sealed trait Token {

    override def toString: String = s"$toChar"

    def toChar: Char = this match {
      case PlusSign     => '+'
      case MinusSign    => '-'
      case TimesSign    => '*'
      case DivisionSign => '/'
      case PowerSign    => '^'
      case LParen       => '('
      case RParen       => ')'
      case DecimalDot   => '.'
    }

  }
  object Token {
    case object PlusSign extends Token
    case object MinusSign extends Token
    case object TimesSign extends Token
    case object DivisionSign extends Token
    case object PowerSign extends Token
    case object LParen extends Token
    case object RParen extends Token
    case object DecimalDot extends Token
  }

  case class Expr(l: Term, reminder: ExprR)
  object Expr {

    def grouped(expr: Expr): Expr = Expr(Term.grouped(expr), EEpsilon)

    def neg(expr: Expr): Expr = Expr(Term.neg(expr), EEpsilon)

    def numb(i: Int): Expr = Expr(Term.numb(i), EEpsilon)
    def numb(d: Double): Expr = Expr(Term.numb(d), EEpsilon)

  }

  sealed trait ExprR
  object ExprR {
    case object EEpsilon extends ExprR
    case class Add(r: Term, reminder: ExprR) extends ExprR
    case class Sub(r: Term, reminder: ExprR) extends ExprR
  }

  case class Term(l: Factor, reminder: TermR)
  object Term {

    def grouped(expr: Expr): Term = Term(Factor.grouped(expr), TEpsilon)

    def neg(expr: Expr): Term = Term(Factor.neg(expr), TEpsilon)

    def numb(i: Int): Term = Term(Factor.numb(i), TEpsilon)
    def numb(d: Double): Term = Term(Factor.numb(d), TEpsilon)

  }

  sealed trait TermR
  object TermR {
    case object TEpsilon extends TermR
    case class Mul(r: Factor, reminder: TermR) extends TermR
    case class Div(r: Factor, reminder: TermR) extends TermR
  }

  sealed trait Factor
  object Factor {
    case class Pow(l: Power, r: Factor) extends Factor
    case class FPower(p: Power) extends Factor

    def grouped(expr: Expr): Factor = FPower(Power.grouped(expr))

    def neg(expr: Expr): Factor = FPower(Minus(Grouped(expr)))

    def numb(i: Int): Factor = FPower(Power.numb(i))
    def numb(d: Double): Factor = FPower(Power.numb(d))

  }

  sealed trait Power
  object Power {
    case class Minus(u: Unary) extends Power
    case class PUnary(u: Unary) extends Power

    def grouped(expr: Expr): Power = PUnary(Grouped(expr))

    def numb(i: Int): Power = if (i < 0) Minus(Natural(-i)) else PUnary(Natural(i))
    def numb(d: Double): Power = if (d < 0.0) Minus(NonNegDecimal(-d)) else PUnary(NonNegDecimal(d))

  }

  sealed trait Unary
  object Unary {
    /* Non-negativity runtime guarantee
      Both Natural.apply and NonNegDecimal.apply require (at runtime) their argument to be non-negative.
      Since
        - this API is meant to be called by parser and test generators, which are supposed to pass proper values, and
        - capturing this constraint in the return type (e.g. `Option[Natural]`) would considerably increase code complexity,
      I opted for a less type-safe runtime guarantee.
     */
    case class Natural(l: Long) extends Unary
    object Natural {
      def apply(x: Long): Natural = {
        require(x >= 0, s"Natural must contain non-negative Long (arg: $x)")
        new Natural(x)
      }
    }
    case class NonNegDecimal(d: Double) extends Unary
    object NonNegDecimal {
      def apply(x: Double): NonNegDecimal = {
        require(x >= 0.0, s"NonNegDecimal must contain non-negative Double (arg: $x)")
        new NonNegDecimal(x)
      }
    }

    case class Grouped(e: Expr) extends Unary
  }

  type EvalResult[A] = Either[EvalError, A]

  sealed trait EvalError
  object EvalError {
    case object DivisionByZero extends EvalError
    case object DivisionUndefined extends EvalError
    case object PowerWithNegativeBase extends EvalError
    case object PowerUndefined extends EvalError
  }

  sealed trait ExprREvalSuccess
  object ExprREvalSuccess {
    case class ExprRAdd(r: Double, reminder: ExprREvalSuccess) extends ExprREvalSuccess
    case class ExprRSub(r: Double, reminder: ExprREvalSuccess) extends ExprREvalSuccess
    case object ExprREpsilon extends ExprREvalSuccess
  }

  sealed trait TermREvalSuccess
  object TermREvalSuccess {
    case class TermRMul(r: Double, reminder: TermREvalSuccess) extends TermREvalSuccess
    case class TermRDiv(r: Double, reminder: TermREvalSuccess) extends TermREvalSuccess
    case object TermREpsilon extends TermREvalSuccess
  }

  sealed trait CalcResult
  object CalcResult {
    case class ParsingError(message: String) extends CalcResult
    case class EvaluationError(error: EvalError) extends CalcResult
    case class Success(value: Double) extends CalcResult
  }

}
