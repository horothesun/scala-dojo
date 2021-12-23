import BinaryTreeUtils.binaryTreeFromList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Gen.{const, listOfN, negNum, oneOf, posNum}

import scala.collection.immutable

final class BinaryTreeSpec extends ScalaCheckSuite {

  val elementGen: Gen[Int] = oneOf(posNum[Int], negNum[Int])
  val elementListGen: Gen[List[Int]] = posNum[Int].flatMap(listOfN(_, elementGen))

  val nilGen: Gen[BinaryTree[Nothing]] = const(Nil)
  val nodeGen: Gen[Node[Int]] =
    elementGen.flatMap(t => binaryTreeGen.flatMap(l => binaryTreeGen.map(r => Node(t, l, r))))
  //  val binaryTreeGen: Gen[BinaryTree[Int]] = lzy(oneOf(nilGen, nodeGen))
  val binaryTreeGen: Gen[BinaryTree[Int]] = elementListGen.map(binaryTreeFromList)

//  property(" ...") {
//    forAll(elementListGen) { as =>
//      collect(as, binaryTreeFromList(as))(true)
//    }
//  }
}

object BinaryTreeUtils {

  def binaryTreeFromList[T]: List[T] => BinaryTree[T] = {
    case ::(head, next) =>
      val halfSize: Int = next.length / 2
      Node(
        head,
        binaryTreeFromList(next.take(halfSize)),
        binaryTreeFromList(next.drop(halfSize))
      )
    case immutable.Nil => Nil
  }
}
