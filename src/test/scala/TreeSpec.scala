import Tree._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Gen.{negNum, oneOf, posNum}
import org.scalacheck.Prop.{forAll, propBoolean}

final class TreeSpec extends ScalaCheckSuite {

  val intGen: Gen[Int] = oneOf(posNum[Int], negNum[Int])
  val treeGen: Gen[Tree[Int]] = Gen.lzy(oneOf(leafGen, branchGen))
  val leafGen: Gen[Leaf[Int]] = intGen.map(Leaf.apply)
  val branchGen: Gen[Branch[Int]] = for { l <- treeGen; r <- treeGen } yield Branch(l, r)

  property("size of Leaf is 1") {
    forAll(leafGen)(size(_) == 1)
  }

  test("size(Branch(Leaf(4), Leaf(5))) is 3") {
    assertEquals(size(Branch(Leaf(4), Leaf(5))), 3)
  }

  test("size(Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))) is 5") {
    assertEquals(size(Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))), 5)
  }

  property("size is always > 0") {
    forAll(treeGen)(size(_) > 0)
  }

  property("size is always odd") {
    def isOdd(i: Int) = i % 2 != 0
    forAll(treeGen)(t => isOdd(size(t)))
  }

}
