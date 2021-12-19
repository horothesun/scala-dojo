import org.scalatest.funsuite.AnyFunSuite

final class CubeCalculatorTest extends AnyFunSuite {
  test("cube(3) is 27") {
    assert(CubeCalculator.cube(3) === 27)
  }
}
