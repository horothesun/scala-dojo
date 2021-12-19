import Sorting.isSorted
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}

object SortingSpec extends Properties("Sorting") {

  val intArrayGen: Gen[Array[Int]] = implicitly[Arbitrary[Array[Int]]].arbitrary
  val sortedIntArrayGen: Gen[Array[Int]] = intArrayGen.map(_.sorted)
  val unsortedIntArrayGen: Gen[Array[Int]] = intArrayGen.filter(as => !(as sameElements as.sorted))

  val intLesserEq: (Int, Int) => Boolean = (l, r) => l <= r

  property("isSorted is true for sorted arrays") = forAll(sortedIntArrayGen)(isSorted(_, intLesserEq))
  property("isSorted is false for unsorted arrays") = forAll(unsortedIntArrayGen)(!isSorted(_, intLesserEq))
}
