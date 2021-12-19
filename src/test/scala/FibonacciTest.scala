import Fibonacci.fib
import org.scalatest.funsuite.AnyFunSuite

final class FibonacciTest extends AnyFunSuite {
  test("fib(0) is 0")(assert(fib(0) === 0))
  test("fib(1) is 1")(assert(fib(1) === 1))
  test("fib(2) is 1")(assert(fib(2) === 1))
  test("fib(3) is 2")(assert(fib(3) === 2))
  test("fib(4) is 3")(assert(fib(4) === 3))
  test("fib(5) is 5")(assert(fib(5) === 5))
  test("fib(6) is 8")(assert(fib(6) === 8))
  test("fib(7) is 13")(assert(fib(7) === 13))
  test("fib(8) is 21")(assert(fib(8) === 21))
  test("fib(9) is 34")(assert(fib(9) === 34))
  test("fib(10) is 55")(assert(fib(10) === 55))
}
