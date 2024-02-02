package exprparsing

import Models._
import Models.ExprR._
import Models.Factor._
import Models.Power._
import Models.TermR._
import Models.Token._
import Models.Unary._
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.Rfc5234.{digit, wsp}
import cats.parse.{Parser, Parser0}
import cats.parse.Parser._

object ExprCodec {

  /* parser */

  // TODO: check how to move from Parser0 to Parser!!!
  def exprP: Parser0[Expr] = (termP <* wspP0, exprRP).mapN(Expr.apply)

  // TODO: check how to move from Parser0 to Parser!!!
  def exprRP: Parser0[ExprR] = {
    def op(token: Token, apply: (Term, ExprR) => ExprR): Parser[ExprR] =
      (char(token.toChar) ~ wspP0) *> (termP <* wspP0, defer0(exprRP)).mapN[ExprR](apply)
    val add = op(PlusSign, Add.apply)
    val sub = op(MinusSign, Sub.apply)
    val epsilon = epsilonP0.as[ExprR](EEpsilon)
    add.orElse(sub).orElse(epsilon)
  }

  // TODO: check how to move from Parser0 to Parser!!!
  def termP: Parser0[Term] = (factorP <* wspP0, defer0(termRP)).mapN(Term.apply)

  // TODO: check how to move from Parser0 to Parser!!!
  def termRP: Parser0[TermR] = {
    def op(token: Token, apply: (Factor, TermR) => TermR): Parser[TermR] =
      (char(token.toChar) ~ wspP0) *> (factorP <* wspP0, defer0(termRP)).mapN[TermR](apply)
    val mul = op(TimesSign, Mul.apply)
    val div = op(DivisionSign, Div.apply)
    val epsilon = epsilonP0.as[TermR](TEpsilon)
    mul.orElse(div).orElse(epsilon)
  }

  def factorP: Parser[Factor] = (powerP <* wspP0).flatMap { p =>
    val pow = (char(PowerSign.toChar) ~ wspP0) *> factorP.map[Factor](Pow(p, _))
    val fPower = Parser.pure[Factor](FPower(p))
    pow.orElse(fPower)
  }

  def powerP: Parser[Power] = {
    val minusP = (char(MinusSign.toChar) ~ wspP0) *> unaryP.map[Power](Minus.apply)
    val pUnary = unaryP.map[Power](PUnary.apply)
    minusP.orElse(pUnary)
  }

  def unaryP: Parser[Unary] = numericP.orElse(defer(groupedP))

  // TODO: fix!!! nat = 0 | [1-9][0-9]+ ; nonNegDec = nat . [0-9]+
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

  // TODO: check!!!
  def epsilonP0: Parser0[Unit] = Parser.pure(())

  /* encode */

  def encode(expr: Expr): String = {
    val lEnc = encode(expr.l)
    expr.reminder match {
      case EEpsilon => lEnc
      case Add(Term(FPower(PUnary(_)), TEpsilon), _) | Sub(Term(FPower(PUnary(_)), TEpsilon), _) =>
        s"$lEnc${encode(expr.reminder)}"
      case Add(_, _) | Sub(_, _) => s"$LParen${encode(expr.l)}$RParen${encode(expr.reminder)}"
    }
  }

  def encode(exprR: ExprR): String = exprR match {
    case EEpsilon                                         => ""
    case Add(Term(FPower(PUnary(u)), TEpsilon), reminder) => s"$PlusSign${encode(u)}${encode(reminder)}"
    case Add(r, reminder)                                 => s"$PlusSign$LParen${encode(r)}$RParen${encode(reminder)}"
    case Sub(Term(FPower(PUnary(u)), TEpsilon), reminder) => s"$MinusSign${encode(u)}${encode(reminder)}"
    case Sub(r, reminder)                                 => s"$MinusSign$LParen${encode(r)}$RParen${encode(reminder)}"
  }

  def encode(term: Term): String = {
    val lEnc = encode(term.l)
    term.reminder match {
      case TEpsilon                                                            => lEnc
      case Mul(FPower(PUnary(_)), TEpsilon) | Div(FPower(PUnary(_)), TEpsilon) => s"$lEnc${encode(term.reminder)}"
      case Mul(_, _) | Div(_, _) => s"$LParen$lEnc$RParen${encode(term.reminder)}"
    }
  }

  def encode(termR: TermR): String = termR match {
    case TEpsilon                         => ""
    case Mul(FPower(PUnary(u)), TEpsilon) => s"$TimesSign${encode(u)}"
    case Mul(r, reminder)                 => s"$TimesSign$LParen${encode(r)}$RParen${encode(reminder)}"
    case Div(FPower(PUnary(u)), TEpsilon) => s"$DivisionSign${encode(u)}"
    case Div(r, reminder)                 => s"$DivisionSign$LParen${encode(r)}$RParen${encode(reminder)}"
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
