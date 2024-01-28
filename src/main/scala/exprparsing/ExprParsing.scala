package exprparsing

import cats.implicits._
import cats.parse.Parser
import cats.parse.Parser._
import exprparsing.ExprParsing.Expr._
import exprparsing.ExprParsing.Factor._
import exprparsing.ExprParsing.NonNegNumber._
import exprparsing.ExprParsing.Power._
import exprparsing.ExprParsing.Term._
import exprparsing.ExprParsing.Unary._

object ExprParsing {

  /*
  Expression pseudo-grammar:
    expr          = term + expr | term - expr | term
    term          = factor * term | factor / term | factor
    factor        = power ^ factor | power
    power         = + expr | - expr | unary
    unary         = ( expr ) | number
    number        = natural | posDecimal
    nonNegDecimal = some digit . some digit
    natural       = some digit
   */

  /* model */

  sealed trait Expr
  object Expr {
    case class Add(l: Term, r: Expr) extends Expr
    case class Sub(l: Term, r: Expr) extends Expr
    case class ETerm(t: Term) extends Expr

    def numb(i: Int): Expr = ETerm(Term.numb(i))
    def numb(d: Double): Expr = ETerm(Term.numb(d))
  }

  sealed trait Term
  object Term {
    case class Mul(l: Factor, r: Term) extends Term
    case class Div(l: Factor, r: Term) extends Term
    case class TFactor(f: Factor) extends Term

    def numb(i: Int): Term = TFactor(Factor.numb(i))
    def numb(d: Double): Term = TFactor(Factor.numb(d))

    def expr(e: Expr): Term = TFactor(FPower(PUnary(Brackets(e))))
  }

  sealed trait Factor
  object Factor {
    case class Pow(l: Power, r: Factor) extends Factor
    case class FPower(p: Power) extends Factor

    def numb(i: Int): Factor = FPower(Power.numb(i))
    def numb(d: Double): Factor = FPower(Power.numb(d))

    def expr(e: Expr): Factor = FPower(PUnary(Brackets(e)))
  }

  sealed trait Power
  object Power {
    case class Plus(e: Expr) extends Power
    case class Minus(e: Expr) extends Power
    case class PUnary(u: Unary) extends Power

    def numb(i: Int): Power = if (i < 0) Minus(Expr.numb(-i)) else PUnary(Unary.numb(i))
    def numb(d: Double): Power = if (d < 0.0) Minus(Expr.numb(-d)) else PUnary(Unary.numb(d))

    def expr(e: Expr): Power = PUnary(Brackets(e))
  }

  sealed trait Unary
  object Unary {
    case class Brackets(e: Expr) extends Unary
    case class UPosNumber(n: NonNegNumber) extends Unary

    def numb(i: Int): Unary = UPosNumber(Natural(i))
    def numb(d: Double): Unary = UPosNumber(NonNegDecimal(d))
  }

  sealed trait NonNegNumber
  object NonNegNumber {
    case class Natural(i: Int) extends NonNegNumber
    object Natural {
      def apply(x: Int): Natural = {
        // non-negativity runtime guarantee: returning Option[Natural] might be an overkill
        require(x >= 0, s"Natural must contain non-negative Int (arg: $x)")
        new Natural(x)
      }
    }
    case class NonNegDecimal(d: Double) extends NonNegNumber
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
    case Brackets(e)   => eval(e)
    case UPosNumber(n) => eval(n)
  }

  def eval(number: NonNegNumber): Option[Double] = number match {
    case Natural(i)       => Some(i)
    case NonNegDecimal(d) => Some(d)
  }

  /* encode */

  def encode(expr: Expr): String = expr match {
    case Add(TFactor(FPower(l)), ETerm(TFactor(FPower(r)))) => s"${encode(l)}+${encode(r)}"
    case Add(TFactor(FPower(l)), r)                         => s"${encode(l)}+(${encode(r)})"
    case Add(l, ETerm(TFactor(FPower(r))))                  => s"(${encode(l)})+${encode(r)}"
    case Add(l, r)                                          => s"(${encode(l)})+(${encode(r)})"
    case Sub(TFactor(FPower(l)), ETerm(TFactor(FPower(r)))) => s"${encode(l)}-${encode(r)}"
    case Sub(TFactor(FPower(l)), r)                         => s"${encode(l)}-(${encode(r)})"
    case Sub(l, ETerm(TFactor(FPower(r))))                  => s"(${encode(l)})-${encode(r)}"
    case Sub(l, r)                                          => s"(${encode(l)})-(${encode(r)})"
    case ETerm(t)                                           => encode(t)
  }

  def encode(term: Term): String = term match {
    case Mul(FPower(l), TFactor(FPower(r))) => s"${encode(l)}*${encode(r)}"
    case Mul(FPower(l), r)                  => s"${encode(l)}*(${encode(r)})"
    case Mul(l, TFactor(FPower(r)))         => s"(${encode(l)})*${encode(r)}"
    case Mul(l, r)                          => s"(${encode(l)})*(${encode(r)})"
    case Div(FPower(l), TFactor(FPower(r))) => s"${encode(l)}/${encode(r)}"
    case Div(FPower(l), r)                  => s"${encode(l)}/(${encode(r)})"
    case Div(l, TFactor(FPower(r)))         => s"(${encode(l)})/${encode(r)}"
    case Div(l, r)                          => s"(${encode(l)})/(${encode(r)})"
    case TFactor(f)                         => encode(f)
  }

  def encode(factor: Factor): String = factor match {
    case Pow(l, FPower(r)) => s"${encode(l)}^${encode(r)}"
    case Pow(l, r)         => s"${encode(l)}^(${encode(r)})"
    case FPower(p)         => encode(p)
  }

  def encode(power: Power): String = power match {
    case Plus(e) => encode(e)
    case Minus(e) =>
      e match {
        case ETerm(TFactor(FPower(PUnary(UPosNumber(Natural(i))))))       => s"(-$i)"
        case ETerm(TFactor(FPower(PUnary(UPosNumber(NonNegDecimal(d)))))) => s"(-$d)"
        case _                                                            => s"-(${encode(e)})"
      }
    case PUnary(u) => encode(u)
  }

  def encode(unary: Unary): String = unary match {
    case Brackets(e)   => s"(${encode(e)})"
    case UPosNumber(n) => encode(n)
  }

  def encode(number: NonNegNumber): String = number match {
    case Natural(i)       => i.toString
    case NonNegDecimal(d) => d.toString
  }

  /* parser */

  def parse(s: String): Parser[Expr] = ???

}
