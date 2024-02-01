package exprparsingrighttoleft

import Models._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Token._
import Models.Unary._
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser, Parser0}
import cats.parse.Parser._
import cats.parse.Rfc5234.{digit, wsp}

object ExprCodec {

  /* https://bnfplayground.pauliankline.com

BNF grammar

<expr>   ::= <term> "+" <expr> | <term> "-" <expr> | <term>
<term>   ::= <factor> "*" <term> | <factor> "/" <term> | <factor>
<factor> ::= <power> "^" <factor> | <power>
<power>  ::= "-" <unary> | <unary>
<unary>  ::= <nonNegDecimal> | <natural> | "(" <expr> ")"

<nonNegDecimal> ::= <digits> "." <digits>
<natural>       ::= <digits>
<digits>        ::= [0-9]+
   */

  /* https://www.cs.princeton.edu/courses/archive/spring20/cos320/LL1/

expr   ::= term '+' expr | term '-' expr | term
term   ::= factor '*' term | factor '/' term | factor
factor ::= power '^' factor | power
power  ::= '-' unary | unary
unary  ::= nonNegDecimal | natural | '(' expr ')'

nonNegDecimal ::= digits '.' digits
natural ::= digits
digits ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
   */

  /* https://planetcalc.com/5600/

expr = term,"+",expr | term,"-",expr | term;
term = factor,"*",term | factor,"/",term | factor;
factor = power,"^",factor | power;
power = "-",unary | unary;
unary = nonNegDecimal | natural | "(",expr,")";

nonNegDecimal = digits,".",digits;
natural = digits;
digits = digit,{digit};
digit = "1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"0";

syntax=expr;
   */

  /* Expression pseudo-grammar:
    expr   = term + expr | term - expr | term
    term   = factor * term | factor / term | factor
    factor = power ^ factor | power
    power  = - unary | unary
    unary  = nonNegDecimal | natural | ( expr )
   */

  /* Left-recursion removal
  https://www.geeksforgeeks.org/removing-direct-and-indirect-left-recursion-in-a-grammar/

  e.g.
    S ⇒ S a | S b | c | d
  becomes
    S ⇒ cS' | dS'
    S' ⇒ ε | aS' | bS'

    -------------------

    // the factor rule won't be inverted because we want ^ to be right-associative

    expr   = expr + term | expr - term | term
    term   = term * factor | term / factor | factor
    factor = power ^ factor | power
    power  = - unary | unary
    unary  = nonNegDecimal | natural | ( expr )

    becomes

    expr    = term expr'
    expr'   = ε | + term expr' | - term expr'
    term    = factor term'
    term'   = ε | * factor term' | / factor term'
    factor  = power ^ factor | power
    power   = - unary | unary
    unary   = nonNegDecimal | natural | ( expr )

    which in BNF form is

<expr>    ::= <term> <exprR>
<exprR>   ::= E | "+" <term> <exprR> | "-" <term> <exprR>
<term>    ::= <factor> <termR>
<termR>   ::= E | "*" <factor> <termR> | "/" <factor> <termR>
<factor>  ::= <power> "^" <factor> | <power>
<power>   ::= "-" <unary> | <unary>
<unary>   ::= <nonNegDecimal> | <natural> | "(" <expr> ")"

<nonNegDecimal> ::= <digits> "." <digits>
<natural>       ::= <digits>
<digits>        ::= [0-9]+

   */

  /* parser */

  def epsilonP: Parser0[Unit] = Parser.pure(())

  def exprP: Parser[Expr] = (termP <* wspP0).flatMap { t =>
    val add = (char(PlusSign.toChar) ~ wspP0) *> exprP.map[Expr](Add(t, _))
    val sub = (char(MinusSign.toChar) ~ wspP0) *> exprP.map[Expr](Sub(t, _))
    val eTerm = Parser.pure[Expr](ETerm(t))
    add.orElse(sub).orElse(eTerm)
  }

  def termP: Parser[Term] = (factorP <* wspP0).flatMap { f =>
    val mul = (char(TimesSign.toChar) ~ wspP0) *> termP.map[Term](Mul(f, _))
    val div = (char(DivisionSign.toChar) ~ wspP0) *> termP.map[Term](Div(f, _))
    val tFactor = Parser.pure[Term](TFactor(f))
    mul.orElse(div).orElse(tFactor)
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
