import Power.{power, powerNonZeroBaseNaturalExp}
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Gen.{negNum, posNum}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Test.Parameters

final class PowerSpec extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(10)

  val posNonZeroNum: Gen[Int] = posNum[Int] suchThat { _ != 0 }
  val negNonZeroNum: Gen[Int] = negNum[Int] suchThat { _ != 0 }

  //  property("power(b, e) is defined if b != 0 or e > 0") {
  //    forAll { (b: Int, e: Int) =>
  //      b != 0 || e > 0 ==> power(b, e).isDefined
  //    }
  //  }

  //  property("powerNonZeroBaseNaturalExp(b, e) > 0 if b > 0") {
  //    forAll(posNonZeroNum, posNum[Int]) { (b, e) =>
  //      powerNonZeroBaseNaturalExp(b, e) > 0
  //    }
  //  }

//  property("powerNonZeroBaseNaturalExp(b, e) > 0 if b < 0 and e is even") {
//    forAll(negNonZeroNum, posNum[Int]) { (b, e) =>
//      e % 2 == 0 ==> powerNonZeroBaseNaturalExp(b, e) > 0
//    }
//  }

//  property("powerNonZeroBaseNaturalExp(b, e) < 0 if b < 0 and e is odd") {
//    forAll(negNonZeroNum, posNum[Int]) { (b, e) =>
//      e % 2 != 0 ==> powerNonZeroBaseNaturalExp(b, e) < 0
//    }
//  }

}
