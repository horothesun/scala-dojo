import Fibonacci.fib
import org.scalacheck.Gen.posNum
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object FibonacciSpec extends Properties("Fibonacci") {
  property("fib(n) + fib(n + 1) = fib(n + 2)") = forAll(posNum[Int]) { n =>
    fib(n) + fib(n + 1) == fib(n + 2)
  }
}
