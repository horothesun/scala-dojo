package exprparsing

import Models.Token._

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

  sealed trait ExprR
  object ExprR {
    case object EEpsilon extends ExprR
    case class Add(r: Term, reminder: ExprR) extends ExprR
    case class Sub(r: Term, reminder: ExprR) extends ExprR
  }

  case class Term(l: Factor, reminder: TermR)

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
  }

  sealed trait Power
  object Power {
    case class Minus(u: Unary) extends Power
    case class PUnary(u: Unary) extends Power
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
    case class Natural(i: Int) extends Unary
    object Natural {
      def apply(x: Int): Natural = {
        require(x >= 0, s"Natural must contain non-negative Int (arg: $x)")
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

}
