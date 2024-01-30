package exprparsing

import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.Parser
import cats.parse.Parser._
import cats.parse.Rfc5234.digit
import ExprParsing.Expr._
import ExprParsing.Factor._
import ExprParsing.Power._
import ExprParsing.Term._
import ExprParsing.Token._
import ExprParsing.Unary._

object ExprParsing {

  /* Expression pseudo-grammar:
    expr   = term + expr | term - expr | term
    term   = factor * term | factor / term | factor
    factor = power ^ factor | power
    power  = ( expr ) | + expr | - expr | unary
    unary  = natural | nonNegDecimal
   */

  /* model */

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

    def numb(i: Int): Expr = ETerm(Term.numb(i))
    def numb(d: Double): Expr = ETerm(Term.numb(d))

    def neg(e: Expr): Expr = ETerm(Term.neg(e))

    def mul(l: Factor, r: Term): Expr = ETerm(Mul(l, r))
    def div(l: Factor, r: Term): Expr = ETerm(Div(l, r))
  }

  sealed trait Term
  object Term {
    case class Mul(l: Factor, r: Term) extends Term
    case class Div(l: Factor, r: Term) extends Term
    case class TFactor(f: Factor) extends Term

    def numb(i: Int): Term = TFactor(Factor.numb(i))
    def numb(d: Double): Term = TFactor(Factor.numb(d))

    def expr(e: Expr): Term = TFactor(Factor.expr(e))

    def neg(e: Expr): Term = TFactor(Factor.neg(e))
  }

  sealed trait Factor
  object Factor {
    case class Pow(l: Power, r: Factor) extends Factor
    case class FPower(p: Power) extends Factor

    def numb(i: Int): Factor = FPower(Power.numb(i))
    def numb(d: Double): Factor = FPower(Power.numb(d))

    def expr(e: Expr): Factor = FPower(Power.expr(e))

    def neg(e: Expr): Factor = FPower(Minus(e))
  }

  sealed trait Power
  object Power {
    case class Plus(e: Expr) extends Power
    case class Minus(e: Expr) extends Power
    case class PUnary(u: Unary) extends Power

    def numb(i: Int): Power = if (i < 0) Minus(Expr.numb(-i)) else PUnary(Natural(i))
    def numb(d: Double): Power = if (d < 0.0) Minus(Expr.numb(-d)) else PUnary(NonNegDecimal(d))

    def expr(e: Expr): Power = Plus(e)
  }

  sealed trait Unary
  object Unary {

    case class Natural(i: Int) extends Unary
    object Natural {
      def apply(x: Int): Natural = {
        // non-negativity runtime guarantee: returning Option[Natural] might be an overkill
        require(x >= 0, s"Natural must contain non-negative Int (arg: $x)")
        new Natural(x)
      }
    }

    case class NonNegDecimal(d: Double) extends Unary
    object NonNegDecimal {
      def apply(x: Double): NonNegDecimal = {
        // non-negativity runtime guarantee: returning Option[Natural] might be an overkill
        require(x >= 0.0, s"NonNegDecimal must contain non-negative Double (arg: $x)")
        new NonNegDecimal(x)
      }
    }

  }

  /* eval
    Info: it returns Option[Double] because of the `/` operator.
   */

  def eval(expr: Expr): Option[Double] = expr match {
    case Add(l, r) => (eval(l), eval(r)).mapN(_ + _)
    case Sub(l, r) => (eval(l), eval(r)).mapN(_ - _)
    case ETerm(t)  => eval(t)
  }

  def eval(term: Term): Option[Double] = term match {
    case Mul(l, r)  => (eval(l), eval(r)).mapN(_ * _)
    case Div(l, r)  => (eval(l), eval(r).filterNot(_ == 0.0)).mapN(_ / _)
    case TFactor(f) => eval(f)
  }

  def eval(factor: Factor): Option[Double] = factor match {
    case Pow(l, r) => (eval(l), eval(r)).mapN(Math.pow)
    case FPower(p) => eval(p)
  }

  def eval(power: Power): Option[Double] = power match {
    case Plus(e)   => eval(e)
    case Minus(e)  => eval(e).map(-_)
    case PUnary(u) => eval(u)
  }

  def eval(unary: Unary): Option[Double] = unary match {
    case Natural(i)       => Some(i)
    case NonNegDecimal(d) => Some(d)
  }

  /* encode */

  def encode(expr: Expr): String = expr match {
    case Add(TFactor(FPower(l)), ETerm(TFactor(FPower(r)))) => s"${encode(l)}$PlusSign${encode(r)}"
    case Add(TFactor(FPower(l)), r)                         => s"${encode(l)}$PlusSign$LParen${encode(r)}$RParen"
    case Add(l, ETerm(TFactor(FPower(r))))                  => s"$LParen${encode(l)}$RParen$PlusSign${encode(r)}"
    case Add(l, r) => s"$LParen${encode(l)}$RParen$PlusSign$LParen${encode(r)}$RParen"
    case Sub(TFactor(FPower(l)), ETerm(TFactor(FPower(r)))) => s"${encode(l)}$MinusSign${encode(r)}"
    case Sub(TFactor(FPower(l)), r)                         => s"${encode(l)}$MinusSign$LParen${encode(r)}$RParen"
    case Sub(l, ETerm(TFactor(FPower(r))))                  => s"$LParen${encode(l)}$RParen$MinusSign${encode(r)}"
    case Sub(l, r) => s"$LParen${encode(l)}$RParen$MinusSign$LParen${encode(r)}$RParen"
    case ETerm(t)  => encode(t)
  }

  def encode(term: Term): String = term match {
    case Mul(FPower(l), TFactor(FPower(r))) => s"${encode(l)}$TimesSign${encode(r)}"
    case Mul(FPower(l), r)                  => s"${encode(l)}$TimesSign$LParen${encode(r)}$RParen"
    case Mul(l, TFactor(FPower(r)))         => s"$LParen${encode(l)}$RParen$TimesSign${encode(r)}"
    case Mul(l, r)                          => s"$LParen${encode(l)}$RParen$TimesSign$LParen${encode(r)}$RParen"
    case Div(FPower(l), TFactor(FPower(r))) => s"${encode(l)}$DivisionSign${encode(r)}"
    case Div(FPower(l), r)                  => s"${encode(l)}$DivisionSign$LParen${encode(r)}$RParen"
    case Div(l, TFactor(FPower(r)))         => s"$LParen${encode(l)}$RParen$DivisionSign${encode(r)}"
    case Div(l, r)                          => s"$LParen${encode(l)}$RParen$DivisionSign$LParen${encode(r)}$RParen"
    case TFactor(f)                         => encode(f)
  }

  def encode(factor: Factor): String = factor match {
    case Pow(l, FPower(r)) => s"${encode(l)}$PowerSign${encode(r)}"
    case Pow(l, r)         => s"${encode(l)}$PowerSign$LParen${encode(r)}$RParen"
    case FPower(p)         => encode(p)
  }

  def encode(power: Power): String = power match {
    case Plus(ETerm(TFactor(FPower(PUnary(u)))))  => encode(u)
    case Plus(e)                                  => s"$LParen${encode(e)}$RParen"
    case Minus(ETerm(TFactor(FPower(PUnary(u))))) => s"$LParen$MinusSign${encode(u)}$RParen"
    case Minus(e)                                 => s"$LParen$MinusSign$LParen${encode(e)}$RParen$RParen"
    case PUnary(u)                                => encode(u)
  }

  def encode(unary: Unary): String = unary match {
    case Natural(i)       => i.toString
    case NonNegDecimal(d) => d.toString
  }

  /* parser */

  def exprP: Parser[Expr] = termP.flatMap { t =>
    val add = (char(PlusSign.toChar) *> exprP).map[Expr](r => Add(t, r))
    val sub = (char(MinusSign.toChar) *> exprP).map[Expr](r => Sub(t, r))
    val eTerm = Parser.pure[Expr](ETerm(t))
    add.orElse(sub).orElse(eTerm)
  }

  def termP: Parser[Term] = factorP.flatMap { f =>
    val mul = (char(TimesSign.toChar) *> termP).map[Term](r => Mul(f, r))
    val div = (char(DivisionSign.toChar) *> termP).map[Term](r => Div(f, r))
    val tFactor = Parser.pure[Term](TFactor(f))
    mul.orElse(div).orElse(tFactor)
  }

  def factorP: Parser[Factor] = powerP.flatMap { p =>
    val pow = (char(PowerSign.toChar) *> factorP).map[Factor](r => Pow(p, r))
    val fPower = Parser.pure[Factor](FPower(p))
    pow.orElse(fPower)
  }

  def powerP: Parser[Power] = unaryP.flatMap { u =>
    val pUnary = Parser.pure[Power](PUnary(u))
    pUnary.orElse(minusP).orElse(plusP)
  }

  def minusP: Parser[Power] = (char(MinusSign.toChar) *> exprP).map(Minus.apply)
  def plusP: Parser[Power] = {
    val plusPrefix = (char(PlusSign.toChar) *> exprP).map[Power](Plus.apply)
    val bracketed = (char(LParen.toChar) *> exprP <* char(RParen.toChar)).map[Power](Plus.apply)
    plusPrefix.orElse(bracketed)
  }

  def unaryP: Parser[Unary] = digitsP.flatMap { ds =>
    val nonNegDecimal = (char(DecimalDot.toChar) *> digitsP)
      .map(rds => ds.append(DecimalDot.toChar).concatNel(rds).mkString_(""))
      .mapFilter[Unary](_.toDoubleOption.map(NonNegDecimal.apply))
    val natural = Parser.pure(ds.mkString_("")).mapFilter[Unary](_.toIntOption.map(Natural.apply))
    nonNegDecimal.orElse(natural)
  }

  def digitsP: Parser[NonEmptyList[Char]] = digit.rep

}
