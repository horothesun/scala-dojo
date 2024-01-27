import ExprParsing.Expr._
import ExprParsing.Term._
import ExprParsing.Factor._
import ExprParsing.Power._
import ExprParsing.Number._
import cats.implicits._

object ExprParsing {

  /*
  Expression pseudo-grammar:
    expr    = term + expr | term - expr | term
    term    = factor * term | factor / term | factor
    factor  = power ^ factor | power
    power   = ( expr ) | number
    number  = decimal | int
    decimal = - (some digit).(some digit) | (some digit).(some digit)
    int     = - nat | nat
    nat     = some digit
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
  }

  sealed trait Factor
  object Factor {
    case class Pow(l: Power, r: Factor) extends Factor
    case class FPower(p: Power) extends Factor

    def numb(i: Int): Factor = FPower(Power.numb(i))
    def numb(d: Double): Factor = FPower(Power.numb(d))
  }

  sealed trait Power
  object Power {
    case class Brackets(e: Expr) extends Power
    case class PNumber(n: Number) extends Power

    def numb(i: Int): Power = PNumber(NInt(i))
    def numb(d: Double): Power = PNumber(NDecimal(d))
  }

  sealed trait Number
  object Number {
    case class NDecimal(d: Double) extends Number
    case class NInt(i: Int) extends Number
  }

  /* eval */

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
    case Brackets(e) => eval(e)
    case PNumber(n)  => eval(n)
  }

  def eval(number: Number): Option[Double] = number match {
    case NDecimal(d) => Some(d)
    case NInt(i)     => Some(i)
  }

  /* encode */

  def encode(expr: Expr): String = expr match {
    case Add(l, r) => s"${encode(l)}+${encode(r)}"
    case Sub(l, r) => s"${encode(l)}-${encode(r)}"
    case ETerm(t)  => encode(t)
  }

  def encode(term: Term): String = term match {
    case Mul(l, r)  => s"${encode(l)}*${encode(r)}"
    case Div(l, r)  => s"${encode(l)}/${encode(r)}"
    case TFactor(f) => encode(f)
  }

  def encode(factor: Factor): String = factor match {
    case Pow(l, r) => s"${encode(l)}+${encode(r)}"
    case FPower(p) => encode(p)
  }

  def encode(power: Power): String = power match {
    case Brackets(e) => s"(${encode(e)})"
    case PNumber(n)  => s"${encode(n)}"
  }

  def encode(number: Number): String = number match {
    case NDecimal(d) => d.toString
    case NInt(i)     => i.toString
  }

}
