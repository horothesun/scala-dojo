import Stack.{pop, push}
import org.scalacheck.Gen.{posNum, listOf, listOfN}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Properties}
import scala.Function.uncurried

object StackSpec extends Properties("Stack") {

  val elementGen: Gen[Int] = arbitrary[Int] suchThat(_ => true)
  val elementsToPushGen: Gen[List[Int]] = posNum[Int].flatMap(listOfN(_, elementGen))
  val stackGen: Gen[Stack[Int]] = listOf(elementGen).map(StackDefault(_))

  property("n pushes followed by n pops leave the Stack unchanged") =
    forAll (elementsToPushGen, stackGen) { (elementsToPush: List[Int], stack: Stack[Int]) =>
      val stackAfterNPushes = elementsToPush.foldRight(stack)(uncurried(push))
      val stackAfterNPushesAndNPops = elementsToPush.foldRight(stackAfterNPushes)((_, s) =>
        pop(s).map(_._2).getOrElse(StackDefault(List[Int]()))
      )
      stack == stackAfterNPushesAndNPops
    }
}
