package exprparsing

import Models.Unary._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ModelsSuite extends ScalaCheckSuite {

  property("Natural(i) throws exception when i < 0") {
    forAll(Gen.negNum[Int]) { i =>
      intercept[java.lang.IllegalArgumentException](Natural(i))
      ()
    }
  }

  property("Natural(i) correctly created when i >= 0") {
    forAll(Gen.oneOf(Gen.const(0), Gen.posNum[Int])) { i =>
      assert(Natural(i).i >= 0)
    }
  }

  property("NonNegDecimal(d) throws exception when d < 0") {
    forAll(Gen.negNum[Double]) { d =>
      intercept[java.lang.IllegalArgumentException](NonNegDecimal(d))
      ()
    }
  }

  property("NonNegDecimal(d) correctly created when d >= 0.0") {
    forAll(Gen.oneOf(Gen.const(0.0), Gen.posNum[Double])) { d =>
      assert(NonNegDecimal(d).d >= 0.0)
    }
  }

}
