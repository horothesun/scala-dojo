package exprparsing

import Models.Factor._
import Models.Power._
import Models.Term._
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

  sealed trait Expr
  object Expr {
    case class Add(l: Term, r: Expr) extends Expr
    case class Sub(l: Term, r: Expr) extends Expr
    case class ETerm(t: Term) extends Expr

    def grouped(e: Expr): Expr = ETerm(Term.grouped(e))

    def mul(l: Factor, r: Term): Expr = ETerm(Mul(l, r))
    def div(l: Factor, r: Term): Expr = ETerm(Div(l, r))

    def pow(l: Power, r: Factor): Expr = ETerm(Term.pow(l, r))

    def neg(e: Expr): Expr = ETerm(Term.neg(e))

    def numb(i: Int): Expr = ETerm(Term.numb(i))
    def numb(d: Double): Expr = ETerm(Term.numb(d))
  }

  sealed trait Term
  object Term {
    case class Mul(l: Factor, r: Term) extends Term
    case class Div(l: Factor, r: Term) extends Term
    case class TFactor(f: Factor) extends Term

    def expr(e: Expr): Term = TFactor(Factor.expr(e))

    def grouped(e: Expr): Term = TFactor(Factor.grouped(e))

    def pow(l: Power, r: Factor): Term = TFactor(Pow(l, r))

    def neg(e: Expr): Term = TFactor(Factor.neg(e))

    def numb(i: Int): Term = TFactor(Factor.numb(i))
    def numb(d: Double): Term = TFactor(Factor.numb(d))
  }

  sealed trait Factor
  object Factor {
    case class Pow(l: Power, r: Factor) extends Factor
    case class FPower(p: Power) extends Factor

    def expr(e: Expr): Factor = FPower(Power.expr(e))

    def grouped(e: Expr): Factor = FPower(Power.grouped(e))

    def neg(e: Expr): Factor = FPower(Minus(Grouped(e)))

    def numb(i: Int): Factor = FPower(Power.numb(i))
    def numb(d: Double): Factor = FPower(Power.numb(d))
  }

  sealed trait Power
  object Power {
    case class Plus(u: Unary) extends Power
    case class Minus(u: Unary) extends Power
    case class PUnary(u: Unary) extends Power

    def expr(e: Expr): Power = PUnary(Grouped(e))

    def grouped(e: Expr): Power = PUnary(Grouped(e))

    def numb(i: Int): Power = if (i < 0) Minus(Natural(-i)) else PUnary(Natural(i))
    def numb(d: Double): Power = if (d < 0.0) Minus(NonNegDecimal(-d)) else PUnary(NonNegDecimal(d))
  }

  sealed trait Unary
  object Unary {
    /* Non-negativity runtime guarantee
      Both Natural.apply and NonNegDecimal.apply require (at runtime) their argument to be non-negative.
      Since
        - this API is meant to be called by parser and by test generators, which are supposed to pass proper values, and
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

  sealed trait CalcResult
  object CalcResult {
    case class ParsingError(message: String) extends CalcResult
    case object DivisionByZeroError extends CalcResult
    case class Success(value: Double) extends CalcResult
  }

}
