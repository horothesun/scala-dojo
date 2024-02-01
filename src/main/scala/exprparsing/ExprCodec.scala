package exprparsing

import Models._
import cats.parse.{Parser, Parser0}

object ExprCodec {

  /*
  Left-recursion removal
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

  def epsilonP: Parser0[Unit] = Parser.pure(()) // TODO: check!!!

}
