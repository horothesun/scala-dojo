import ExtraLongFactorials._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ExtraLongFactorialsSpec extends ScalaCheckSuite {

  test("extraLongFactorials(25) = 15511210043330985984000000") {
    assertEquals(extraLongFactorials(25), BigInt("15511210043330985984000000"))
  }

  property("extraLongFactorials(n) >= 1, for 1 <= n <= 100") {
    forAll(Gen.chooseNum(minT = 1, maxT = 100)) { n =>
      assert(extraLongFactorials(n) >= 1)
    }
  }

}
