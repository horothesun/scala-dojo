import Fibonacci.fib
import munit.ScalaCheckSuite
import org.scalacheck.Gen.posNum
import org.scalacheck.Prop.{forAll, propBoolean}

final class FibonacciSpec extends ScalaCheckSuite {

  test("fib(0) is 0")(assertEquals(fib(0), 0))
  test("fib(1) is 1")(assertEquals(fib(1), 1))
  test("fib(2) is 1")(assertEquals(fib(2), 1))
  test("fib(3) is 2")(assertEquals(fib(3), 2))
  test("fib(4) is 3")(assertEquals(fib(4), 3))
  test("fib(5) is 5")(assertEquals(fib(5), 5))
  test("fib(6) is 8")(assertEquals(fib(6), 8))
  test("fib(7) is 13")(assertEquals(fib(7), 13))
  test("fib(8) is 21")(assertEquals(fib(8), 21))
  test("fib(9) is 34")(assertEquals(fib(9), 34))
  test("fib(10) is 55")(assertEquals(fib(10), 55))

  property("fib(n) + fib(n + 1) = fib(n + 2)") {
    forAll(posNum[Int]) { n =>
      fib(n) + fib(n + 1) == fib(n + 2)
    }
  }
}
