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

    def eNumb(i: Int): Expr = ETerm(tNumb(i))
    def eNumb(d: Double): Expr = ETerm(tNumb(d))
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

    def tNumb(i: Int): Term = TFactor(fNumb(i))
    def tNumb(d: Double): Term = TFactor(fNumb(d))
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

    def fNumb(i: Int): Factor = FPower(pNumb(i))
    def fNumb(d: Double): Factor = FPower(pNumb(d))
  }

  sealed trait Power {
    def eval: Option[Double] = this match {
      case Brackets(e) => e.eval
      case Numb(n)     => n.eval
    }
  }
  object Power {
    case class Brackets(e: Expr) extends Power
    case class Numb(n: Number) extends Power

    def pNumb(i: Int): Power = Numb(NInt(i))
    def pNumb(d: Double): Power = Numb(NDecimal(d))
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
