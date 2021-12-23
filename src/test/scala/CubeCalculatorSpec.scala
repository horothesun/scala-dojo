import CubeCalculator.cube
import munit.ScalaCheckSuite
import org.scalacheck.Prop.{forAll, propBoolean}

final class CubeCalculatorSpec extends ScalaCheckSuite {

  test("cube(3) is 27") {
    assert(CubeCalculator.cube(3) == 27)
  }

  property("cube(a) is positive if a is too") {
    forAll { a: Int => a > 0 ==> cube(a) > 0 }
  }

  property("cube(a) is negative if a is too") {
    forAll { a: Int => a < 0 ==> cube(a) < 0 }
  }

  property("cube(a) is non-negative iff a is non-negative") {
    forAll { a: Int => a >= 0 == cube(a) >= 0 }
  }

}
