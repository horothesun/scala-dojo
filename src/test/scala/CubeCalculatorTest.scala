import org.scalatest.funsuite.AnyFunSuite

class CubeCalculatorTest extends AnyFunSuite {
  test("CubeCalculator.cube(3) is 27") {
    assert(CubeCalculator.cube(3) === 27)
  }
}
