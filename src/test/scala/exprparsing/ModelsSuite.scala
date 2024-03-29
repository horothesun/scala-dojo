package exprparsing

import ExprGenerators.nonNegNum
import Models.Unary._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ModelsSuite extends ScalaCheckSuite {

  property("Natural(l) throws exception when l < 0L") {
    forAll(Gen.negNum[Long]) { l =>
      intercept[java.lang.IllegalArgumentException](Natural(l))
      ()
    }
  }

  property("Natural(l) correctly created when l >= 0L") {
    forAll(nonNegNum[Long])(l => assert(Natural(l).l >= 0L))
  }

  property("NonNegDecimal(d) throws exception when d < 0.0") {
    forAll(Gen.negNum[Double]) { d =>
      intercept[java.lang.IllegalArgumentException](NonNegDecimal(d))
      ()
    }
  }

  property("NonNegDecimal(d) correctly created when d >= 0.0") {
    forAll(nonNegNum[Double])(d => assert(NonNegDecimal(d).d >= 0.0))
  }

}
