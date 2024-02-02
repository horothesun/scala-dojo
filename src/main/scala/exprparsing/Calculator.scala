package exprparsing

import ExprCodec._
import ExprEval._
import Models._
import Models.CalcResult._

object Calculator {

  def calc(expression: String): CalcResult = exprP.parse(preprocess(expression)) match {
    case Left(err)        => ParsingError(s"$err")
    case Right(("", res)) => eval(res).fold[CalcResult](EvaluationError.apply, Success.apply)
    case Right((s, _))    => ParsingError(s"Input not fully consumed: $s")
  }

  // TODO: capture any leading and trailing spaces with exprP!!!
  def preprocess(expression: String): String = expression.trim

}
