package exprparsing

import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.Parser
import cats.parse.Parser._
import cats.parse.Rfc5234.digit
import exprparsing.ExprParsing.Expr._
import exprparsing.ExprParsing.Factor._
import exprparsing.ExprParsing.Power._
import exprparsing.ExprParsing.Term._
import exprparsing.ExprParsing.Unary._

object ExprParsing {

  /* Expression pseudo-grammar:
    expr   = term + expr | term - expr | term
    term   = factor * term | factor / term | factor
    factor = power ^ factor | power
    power  = ( expr ) | + expr | - expr | unary
    unary  = natural | nonNegDecimal
   */

  /* model */

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
    case Plus(ETerm(TFactor(FPower(PUnary(u)))))  => encode(u)
    case Plus(e)                                  => s"(${encode(e)})"
    case Minus(ETerm(TFactor(FPower(PUnary(u))))) => s"(-${encode(u)})"
    case Minus(e)                                 => s"(-(${encode(e)}))"
    case PUnary(u)                                => encode(u)
  }

  def encode(unary: Unary): String = unary match {
    case Natural(i)       => i.toString
    case NonNegDecimal(d) => d.toString
  }

  /* parser */

  def exprP: Parser[Expr] = termP.flatMap { t =>
    val add = (char('+') *> exprP).map[Expr](r => Add(t, r))
    val sub = (char('-') *> exprP).map[Expr](r => Sub(t, r))
    val eTerm = Parser.pure[Expr](ETerm(t))
    add.orElse(sub).orElse(eTerm)
  }

  def termP: Parser[Term] = factorP.flatMap { f =>
    val mul = (char('*') *> termP).map[Term](r => Mul(f, r))
    val div = (char('/') *> termP).map[Term](r => Div(f, r))
    val tFactor = Parser.pure[Term](TFactor(f))
    mul.orElse(div).orElse(tFactor)
  }

  def factorP: Parser[Factor] = powerP.flatMap { p =>
    val pow = (char('^') *> factorP).map[Factor](r => Pow(p, r))
    val fPower = Parser.pure[Factor](FPower(p))
    pow.orElse(fPower)
  }

  def powerP: Parser[Power] = Parser.oneOf(unaryP.map(PUnary.apply) :: minusP :: plusP :: Nil)

  def plusP: Parser[Power] = {
    val bracketed = (char('(') *> exprP <* char(')')).map[Power](Plus.apply)
    val plusPrefix = (char('+') *> exprP).map[Power](Plus.apply)
    bracketed.orElse(plusPrefix)
  }
  def minusP: Parser[Power] = (char('-') *> exprP).map(Minus.apply)

  def unaryP: Parser[Unary] = digits.flatMap { ds =>
    val nonNegDecimal = (charIn('.'), digits).mapN { case (dot, rds) => ds.append(dot).concatNel(rds).mkString_("") }
      .mapFilter[Unary](_.toDoubleOption.map(NonNegDecimal.apply))
    val natural = Parser.pure(ds.mkString_("")).mapFilter[Unary](_.toIntOption.map(Natural.apply))
    nonNegDecimal.orElse(natural)
  }

  def digits: Parser[NonEmptyList[Char]] = digit.rep

  /*
  1) Will the input expressions follow the PEMDAS convention?
  Higher to lower priority:
  - Parenthesis
  - Exponentiation
  - Multiplication/Division (same priority, left-to-right)
  - Addition/Subtraction (same priority, left-to-right)

  E.g.: "-1/2*3-(4/5-3)" means "(((-1)/2)*3)-((4/5)-3)"

  2) Is the `*` symbol going to be always explicit?

  E.g.: no shortcuts like "2(1+3)" instead of "2*(1+3)"

   */

}
