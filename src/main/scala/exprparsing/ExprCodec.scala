package exprparsing

import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser, Parser0}
import cats.parse.Parser._
import cats.parse.Rfc5234.{digit, wsp}
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
    power  = - unary | unary
    unary  = nonNegDecimal | natural | ( expr )
   */

  /* parser */

  def exprP: Parser[Expr] = (termP <* wspP0).flatMap { t =>
    val add = (char(PlusSign.toChar) ~ wspP0) *> exprP.map[Expr](r => Add(t, r))
    val sub = (char(MinusSign.toChar) ~ wspP0) *> exprP.map[Expr](r => Sub(t, r))
    val eTerm = Parser.pure[Expr](ETerm(t))
    add.orElse(sub).orElse(eTerm)
  }

  def termP: Parser[Term] = (factorP <* wspP0).flatMap { f =>
    val mul = (char(TimesSign.toChar) ~ wspP0) *> termP.map[Term](r => Mul(f, r))
    val div = (char(DivisionSign.toChar) ~ wspP0) *> termP.map[Term](r => Div(f, r))
    val tFactor = Parser.pure[Term](TFactor(f))
    mul.orElse(div).orElse(tFactor)
  }

  def factorP: Parser[Factor] = (powerP <* wspP0).flatMap { p =>
    val pow = (char(PowerSign.toChar) ~ wspP0) *> factorP.map[Factor](r => Pow(p, r))
    val fPower = Parser.pure[Factor](FPower(p))
    pow.orElse(fPower)
  }

  def powerP: Parser[Power] = {
    val minusP = (char(MinusSign.toChar) ~ wspP0) *> unaryP.map[Power](u => Minus(u))
    val pUnary = unaryP.map[Power](PUnary.apply)
    minusP.orElse(pUnary)
  }

  def unaryP: Parser[Unary] = numericP.orElse(defer(groupedP))

  def numericP: Parser[Unary] = digitsP.flatMap { ds =>
    val nonNegDecimal = char(DecimalDot.toChar) *>
      digitsP
        .map(rds => ds.append(DecimalDot.toChar).concatNel(rds).mkString_(""))
        .mapFilter[Unary](_.toDoubleOption.map(NonNegDecimal.apply))
    val natural = Parser.pure(ds.mkString_("")).mapFilter[Unary](_.toIntOption.map(Natural.apply))
    nonNegDecimal.orElse(natural)
  }

  def groupedP: Parser[Unary] = for {
    _ <- char(LParen.toChar)
    _ <- wspP0
    e <- exprP
    _ <- wspP0
    _ <- char(RParen.toChar)
  } yield Grouped(e)

  def digitsP: Parser[NonEmptyList[Char]] = digit.rep

  def wspP0: Parser0[Unit] = wsp.rep0.void

  /* encode */

  def encode(expr: Expr): String = expr match {
    case Add(TFactor(FPower(PUnary(l))), ETerm(TFactor(FPower(PUnary(r))))) => s"${encode(l)}$PlusSign${encode(r)}"
    case Add(l, ETerm(TFactor(FPower(PUnary(r))))) => s"$LParen${encode(l)}$RParen$PlusSign${encode(r)}"
    case Add(TFactor(FPower(PUnary(l))), r)        => s"${encode(l)}$PlusSign$LParen${encode(r)}$RParen"
    case Add(l, r)                                 => s"$LParen${encode(l)}$RParen$PlusSign$LParen${encode(r)}$RParen"
    case Sub(TFactor(FPower(PUnary(l))), ETerm(TFactor(FPower(PUnary(r))))) => s"${encode(l)}$MinusSign${encode(r)}"
    case Sub(l, ETerm(TFactor(FPower(PUnary(r))))) => s"$LParen${encode(l)}$RParen$MinusSign${encode(r)}"
    case Sub(TFactor(FPower(PUnary(l))), r)        => s"${encode(l)}$MinusSign$LParen${encode(r)}$RParen"
    case Sub(l, r)                                 => s"$LParen${encode(l)}$RParen$MinusSign$LParen${encode(r)}$RParen"
    case ETerm(t)                                  => encode(t)
  }

  def encode(term: Term): String = term match {
    case Mul(FPower(PUnary(l)), TFactor(FPower(PUnary(r)))) => s"${encode(l)}$TimesSign${encode(r)}"
    case Mul(l, TFactor(FPower(PUnary(r))))                 => s"$LParen${encode(l)}$RParen$TimesSign${encode(r)}"
    case Mul(FPower(PUnary(l)), r)                          => s"${encode(l)}$TimesSign$LParen${encode(r)}$RParen"
    case Mul(l, r) => s"$LParen${encode(l)}$RParen$TimesSign$LParen${encode(r)}$RParen"
    case Div(FPower(PUnary(l)), TFactor(FPower(PUnary(r)))) => s"${encode(l)}$DivisionSign${encode(r)}"
    case Div(l, TFactor(FPower(PUnary(r))))                 => s"$LParen${encode(l)}$RParen$DivisionSign${encode(r)}"
    case Div(FPower(PUnary(l)), r)                          => s"${encode(l)}$DivisionSign$LParen${encode(r)}$RParen"
    case Div(l, r)  => s"$LParen${encode(l)}$RParen$DivisionSign$LParen${encode(r)}$RParen"
    case TFactor(f) => encode(f)
  }

  def encode(factor: Factor): String = factor match {
    case Pow(PUnary(l), FPower(PUnary(r))) => s"${encode(l)}$PowerSign${encode(r)}"
    case Pow(l, FPower(PUnary(r)))         => s"$LParen${encode(l)}$RParen$PowerSign${encode(r)}"
    case Pow(PUnary(l), r)                 => s"${encode(l)}$PowerSign$LParen${encode(r)}$RParen"
    case Pow(l, r)                         => s"$LParen${encode(l)}$RParen$PowerSign$LParen${encode(r)}$RParen"
    case FPower(p)                         => encode(p)
  }

  def encode(power: Power): String = power match {
    case Minus(u)  => s"$MinusSign${encode(u)}"
    case PUnary(u) => encode(u)
  }

  def encode(unary: Unary): String = unary match {
    case Natural(i)       => i.toString
    case NonNegDecimal(d) => d.toString
    case Grouped(e)       => s"$LParen${encode(e)}$RParen"
  }

}
