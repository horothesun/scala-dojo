package exprparsing

import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.Parser
import cats.parse.Parser._
import cats.parse.Rfc5234.digit
import Models._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Token._
import Models.Unary._

object ExprCodec {

  /* Expression pseudo-grammar:
    expr   = term + expr | term - expr | term
    term   = factor * term | factor / term | factor
    factor = power ^ factor | power
    power  = ( expr ) | + expr | - expr | unary
    unary  = natural | nonNegDecimal
   */

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

}
