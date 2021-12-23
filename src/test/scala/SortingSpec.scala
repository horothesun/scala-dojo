import Sorting.isSorted
import munit.ScalaCheckSuite
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen}

final class SortingSpec extends ScalaCheckSuite {

  val intArrayGen: Gen[Array[Int]] = implicitly[Arbitrary[Array[Int]]].arbitrary
  val sortedIntArrayGen: Gen[Array[Int]] = intArrayGen.map(_.sorted)
  val unsortedIntArrayGen: Gen[Array[Int]] = intArrayGen.filter(as => !(as sameElements as.sorted))

  val intLesserEq: (Int, Int) => Boolean = (l, r) => l <= r

  property("isSorted is true for sorted arrays") {
    forAll(sortedIntArrayGen)(isSorted(_, intLesserEq))
  }

  property("isSorted is false for unsorted arrays") {
    forAll(unsortedIntArrayGen)(!isSorted(_, intLesserEq))
  }

}
