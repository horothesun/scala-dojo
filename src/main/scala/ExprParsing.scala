import ExprParsing.Expr._
import ExprParsing.Term._
import ExprParsing.Factor._
import ExprParsing.Power._
import ExprParsing.Number._
import cats.implicits._

object ExprParsing {

  /*
  expr    = term + expr | term - expr | term
  term    = factor * term | factor / term | factor
  factor  = power ^ factor | power
  power   = ( expr ) | number
  number  = decimal | int
  decimal = - (some digit).(some digit) | (some digit).(some digit)
  int     = - nat | nat
  nat     = some digit
   */

  sealed trait Expr {
    def eval: Option[Double] = this match {
      case Add(l, r) => (l.eval, r.eval).mapN(_ + _)
      case Sub(l, r) => (l.eval, r.eval).mapN(_ - _)
      case ETerm(t)  => t.eval
    }
  }
  object Expr {
    case class Add(l: Term, r: Expr) extends Expr
    case class Sub(l: Term, r: Expr) extends Expr
    case class ETerm(t: Term) extends Expr

    def numb(i: Int): Expr = ETerm(Term.numb(i))
    def numb(d: Double): Expr = ETerm(Term.numb(d))
  }

  sealed trait Term {
    def eval: Option[Double] = this match {
      case Mul(l, r)  => (l.eval, r.eval).mapN(_ * _)
      case Div(l, r)  => (l.eval, r.eval.filterNot(_ == 0.0)).mapN(_ / _)
      case TFactor(f) => f.eval
    }
  }
  object Term {
    case class Mul(l: Factor, r: Term) extends Term
    case class Div(l: Factor, r: Term) extends Term
    case class TFactor(f: Factor) extends Term

    def numb(i: Int): Term = TFactor(Factor.numb(i))
    def numb(d: Double): Term = TFactor(Factor.numb(d))
  }

  sealed trait Factor {
    def eval: Option[Double] = this match {
      case Pow(l, r) => (l.eval, r.eval).mapN(Math.pow)
      case FPower(p) => p.eval
    }
  }
  object Factor {
    case class Pow(l: Power, r: Factor) extends Factor
    case class FPower(p: Power) extends Factor

    def numb(i: Int): Factor = FPower(Power.numb(i))
    def numb(d: Double): Factor = FPower(Power.numb(d))
  }

  sealed trait Power {
    def eval: Option[Double] = this match {
      case Brackets(e) => e.eval
      case PNumber(n)     => n.eval
    }
  }
  object Power {
    case class Brackets(e: Expr) extends Power
    case class PNumber(n: Number) extends Power

    def numb(i: Int): Power = PNumber(NInt(i))
    def numb(d: Double): Power = PNumber(NDecimal(d))
  }

  sealed trait Number {
    def eval: Option[Double] = this match {
      case NDecimal(d) => Some(d)
      case NInt(i)     => Some(i)
    }
  }
  object Number {
    case class NDecimal(d: Double) extends Number
    case class NInt(i: Int) extends Number
  }

}
