package exprparsing

import cats.implicits._
import Models._
import Models.Expr._
import Models.Factor._
import Models.Power._
import Models.Term._
import Models.Unary._

object ExprEval {

  // `eval` methods return Option[Double] because division is supported

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
    case Minus(e)  => eval(e).map(-_)
    case PUnary(u) => eval(u)
  }

  def eval(unary: Unary): Option[Double] = unary match {
    case Natural(i)       => Some(i)
    case NonNegDecimal(d) => Some(d)
    case Grouped(e)       => eval(e)
  }

}
