package exprparsing

import Models._
import Models.ExprR._
import Models.TermR._
import Models.Factor._
import Models.Power._
import Models.Unary._
import Models.EvalError._
import Models.ExprREvalSuccess._
import Models.TermREvalSuccess._
import cats.implicits._
import scala.annotation.tailrec

object ExprEval {

  // `eval` methods return EvalResult[_] because division and power are supported

  def eval(expr: Expr): EvalResult[Double] = {
    @tailrec
    def aux(acc: Double, reminderRes: ExprREvalSuccess): EvalResult[Double] = reminderRes match {
      case ExprRAdd(r, reminder) => aux(acc + r, reminder)
      case ExprRSub(r, reminder) => aux(acc - r, reminder)
      case ExprREpsilon          => Right(acc)
    }

    (eval(expr.l), eval(expr.reminder)).flatMapN(aux)
  }

  def eval(exprR: ExprR): EvalResult[ExprREvalSuccess] = exprR match {
    case EEpsilon         => Right(ExprREpsilon)
    case Add(r, reminder) => (eval(r), eval(reminder)).mapN(ExprRAdd.apply)
    case Sub(r, reminder) => (eval(r), eval(reminder)).mapN(ExprRSub.apply)
  }

  def eval(term: Term): EvalResult[Double] = {
    @tailrec
    def aux(acc: Double, reminderRes: TermREvalSuccess): EvalResult[Double] = reminderRes match {
      case TermRMul(r, reminder) => aux(acc * r, reminder)
      case TermRDiv(r, reminder) =>
        (acc, r) match {
          case (0.0, 0.0) => Left(DivisionUndefined)
          case (_, 0.0)   => Left(DivisionByZero)
          case (ld, rd)   => aux(ld / rd, reminder)
        }
      case TermREpsilon => Right(acc)
    }

    (eval(term.l), eval(term.reminder)).flatMapN(aux)
  }

  def eval(termR: TermR): EvalResult[TermREvalSuccess] = termR match {
    case TEpsilon         => Right(TermREpsilon)
    case Mul(r, reminder) => (eval(r), eval(reminder)).mapN(TermRMul.apply)
    case Div(r, reminder) => (eval(r), eval(reminder)).mapN(TermRDiv.apply)
  }

  // TODO: check Left-returning cases!!!
  def eval(factor: Factor): EvalResult[Double] = factor match {
    case Pow(l, r) =>
      (eval(l), eval(r)).flatMapN {
        case (0.0, rd) if rd < 0.0 => Left(PowerUndefined)
        case (ld, _) if ld < 0.0   => Left(PowerWithNegativeBase)
        case (ld, rd)              => Right(Math.pow(ld, rd))
      }
    case FPower(p) => eval(p)
  }

  def eval(power: Power): EvalResult[Double] = power match {
    case Minus(e)  => eval(e).map(-_)
    case PUnary(u) => eval(u)
  }

  def eval(unary: Unary): EvalResult[Double] = unary match {
    case Natural(l)       => Right(l.toDouble)
    case NonNegDecimal(d) => Right(d)
    case Grouped(e)       => eval(e)
  }

}
