package exprparsingrighttoleft

import Models._
import Models.EvalError._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Unary._
import cats.syntax.all._

object ExprEval {

  // `eval` methods return Either[EvalError, Double] because division and power are supported

  def eval(expr: Expr): Either[EvalError, Double] = expr match {
    case Add(l, r) => (eval(l), eval(r)).mapN(_ + _)
    case Sub(l, r) => (eval(l), eval(r)).mapN(_ - _)
    case ETerm(t)  => eval(t)
  }

  def eval(term: Term): Either[EvalError, Double] = term match {
    case Mul(l, r) => (eval(l), eval(r)).mapN(_ * _)
    case Div(l, r) =>
      (eval(l), eval(r)).flatMapN {
        case (0.0, 0.0) => Left(DivisionUndefined)
        case (_, 0.0)   => Left(DivisionByZero)
        case (ld, rd)   => Right(ld / rd)
      }
    case TFactor(f) => eval(f)
  }

  // TODO: check!!!
  def eval(factor: Factor): Either[EvalError, Double] = factor match {
    case Pow(l, r) =>
      (eval(l), eval(r)).flatMapN {
        case (0.0, rd) if rd < 0.0 => Left(PowerUndefined)
        case (ld, _) if ld < 0.0   => Left(PowerWithNegativeBase)
        case (ld, rd)              => Right(Math.pow(ld, rd))
      }
    case FPower(p) => eval(p)
  }

  def eval(power: Power): Either[EvalError, Double] = power match {
    case Minus(e)  => eval(e).map(-_)
    case PUnary(u) => eval(u)
  }

  def eval(unary: Unary): Either[EvalError, Double] = unary match {
    case Natural(i)       => Right(i)
    case NonNegDecimal(d) => Right(d)
    case Grouped(e)       => eval(e)
  }

}
