import CubeCalculator.cube
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}

object CubeCalculatorSpec extends Properties("CubeCalculator") {
  property("cube(a) is positive if a is too") = forAll { a: Int =>
    a > 0 ==> cube(a) > 0
  }
  property("cube(a) is negative if a is too") = forAll { a: Int =>
    a < 0 ==> cube(a) < 0
  }
  property("cube(a) is non-negative iff a is non-negative") = forAll { a: Int =>
    a >= 0 == cube(a) >= 0
  }
}
