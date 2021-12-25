import Tree._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Gen.{const, lzy, negNum, oneOf, posNum}
import org.scalacheck.Prop.{forAll, propBoolean}

final class TreeSpec extends ScalaCheckSuite {

  val intGen: Gen[Int] = oneOf(posNum[Int], negNum[Int])
  val treeGen: Gen[Tree[Int]] = lzy(oneOf(leafGen, branchGen))
  val leafGen: Gen[Leaf[Int]] = intGen.map(Leaf.apply)
  val branchGen: Gen[Branch[Int]] = for { l <- treeGen; r <- treeGen } yield Branch(l, r)

  def treeGenFromValue(value: Int): Gen[Tree[Int]] = lzy(oneOf(leafGenFromValue(value), branchGenFromValue(value)))
  def leafGenFromValue(value: Int): Gen[Leaf[Int]] = const(Leaf(value))
  def branchGenFromValue(value: Int): Gen[Branch[Int]] = for {
    l <- treeGenFromValue(value)
    r <- treeGenFromValue(value)
  } yield Branch(l, r)

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

  test("maximum(Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))) is 6") {
    assertEquals(maximum(Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))), 6)
  }

  property("maximum of Leaf(i) is i") {
    forAll(leafGen)(l => maximum(l) == l.value)
  }

  property("maximum of left Branch made of 1s and right Branch made of 2s is 2") {
    forAll(treeGenFromValue(1), treeGenFromValue(2)) { (l, r) =>
      maximum(Branch(l, r)) == 2
    }
  }

  property("maximum of left Branch made of 2s and right Branch made of 1s is 2") {
    forAll(treeGenFromValue(2), treeGenFromValue(1)) { (l, r) =>
      maximum(Branch(l, r)) == 2
    }
  }

}
