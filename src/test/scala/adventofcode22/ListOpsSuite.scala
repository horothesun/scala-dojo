package adventofcode22

import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import ListOps._
import ListOpsSuite._

class ListOpsSuite extends ScalaCheckSuite {

  test("splitListBy[Int](elem = 0) on empty list returns 1 empty list") {
    assertEquals(splitListBy[Int](0)(List.empty), List(List.empty))
  }

  test("splitListBy[Int](elem = 0)(List(0)) returns 2 empty lists") {
    assertEquals(splitListBy[Int](0)(List(0)), List(List.empty, List.empty))
  }

  test("splitListBy[Int](elem = 0) on non-empty list") {
    assertEquals(
      splitListBy[Int](0)(List(1, 2, 0, 3, 0, 4, 5, 6)),
      List(List(1, 2), List(3), List(4, 5, 6))
    )
  }

  property("splitListBy[Char] behaves like String.split with non-empty splits") {
    forAll(separatorAndNonEmptySplitsStringGen(alphabet = NonEmptyList.of('a', 'b', 'c', 'd'))) { case (c, s) =>
      assertEquals(
        splitListBy[Char](c)(s.toList),
        s.split(s"$c").map(_.toList).toList
      )
    }
  }

  test("updatedOption(index = 1, elem = 'z')(List('a','b','c')) returns Some(List('a','z','c'))") {
    assertEquals(
      updatedOption(index = 1, elem = 'z')(List('a', 'b', 'c')),
      Some(List('a', 'z', 'c'))
    )
  }

  property("updatedOption(index, elem)(as) with 0 <= index < as.length always returns list of same length") {
    forAll(updatedOptionInputGen) { case (index, elem, as) =>
      assertEquals(updatedOption(index, elem)(as).map(_.length), Some(as.length))
    }
  }

  property("updatedOption(index, elem)(as) with index < 0 always returns None") {
    forAll(Gen.negNum[Int], Gen.alphaChar, Gen.listOf(Gen.alphaChar)) { case (neg, elem, as) =>
      assertEquals(updatedOption(index = neg, elem)(as), None)
    }
  }

  property("updatedOption(index, elem)(as) with index >= as.length always returns None") {
    forAll(Gen.posNum[Int], Gen.alphaChar, Gen.listOf(Gen.alphaChar)) { case (pos, elem, as) =>
      assertEquals(updatedOption(index = (pos - 1) + as.length, elem)(as), None)
    }
  }

}
object ListOpsSuite {

  def separatorAndNonEmptySplitsStringGen(alphabet: NonEmptyList[Char]): Gen[(Char, String)] = {
    val NonEmptyList(c, rest) = alphabet
    val nonEmptyStringFromRestGen = Gen.chooseNum(1, 5).flatMap(n => Gen.stringOfN(n, Gen.oneOf(rest)))
    Gen.zip(
      Gen.const(c),
      Gen.chooseNum(1, 10).flatMap(n => Gen.listOfN(n, nonEmptyStringFromRestGen).map(_.mkString(s"$c")))
    )
  }

  def updatedOptionInputGen: Gen[(Int, Char, List[Char])] =
    Gen
      .chooseNum(1, 20)
      .flatMap(size => Gen.chooseNum(0, size - 1).map(index => (size, index)))
      .flatMap { case (size, index) =>
        Gen
          .zip(
            Gen.alphaChar,
            Gen.listOfN(size, Gen.alphaChar)
          )
          .map { case (a, as) => (index, a, as) }
      }

}
