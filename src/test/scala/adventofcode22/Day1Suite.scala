package adventofcode22

import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import Day1._
import Day1Suite._

class Day1Suite extends ScalaCheckSuite {

  test("splitListBy[Int](0) on empty list returns 1 empty list") {
    assertEquals(splitListBy[Int](0)(List.empty), List(List.empty))
  }

  test("splitListBy[Int](0)(List(0)) returns 2 empty lists") {
    assertEquals(splitListBy[Int](0)(List(0)), List(List.empty, List.empty))
  }

  test("splitListBy[Int](0) on non-empty list") {
    assertEquals(
      splitListBy[Int](0)(List(1, 2, 0, 3, 0, 4, 5, 6)),
      List(List(1, 2), List(3), List(4, 5, 6))
    )
  }

  property("splitListBy[Char] behaves like String.split with non-empty splits") {
    forAll(splitListByInputGen(alphabet = NonEmptyList.of('a', 'b', 'c', 'd'))) { case (c, s) =>
      assertEquals(
        splitListBy[Char](c)(s.toList),
        s.split(s"$c").map(_.toList).toList
      )
    }
  }

  test("total Calories carried by the Elf with most calories (small input)") {
    val input =
      """
        |1000
        |2000
        |3000
        |
        |4000
        |
        |5000
        |6000
        |
        |7000
        |8000
        |9000
        |
        |10000
        |""".stripMargin.linesIterator.toList
    assertEquals(getElfWithMostCaloriesTotalCalories(input), Some(24_000))
  }

  test("total Calories carried by the Elf with most calories (BIG input)") {
    val input = getLinesFromFile("src/test/scala/adventofcode22/day1_input.txt")
    assertEquals(getElfWithMostCaloriesTotalCalories(input), Some(70_764))
  }

  test("total Calories carried by top 3 Elves with most calories (small input)") {
    val input =
      """
        |1000
        |2000
        |3000
        |
        |4000
        |
        |5000
        |6000
        |
        |7000
        |8000
        |9000
        |
        |10000
        |""".stripMargin.linesIterator.toList
    assertEquals(getTop3ElvesWithMostCaloriesTotalCalories(input), Some(45_000))
  }

  test("total Calories carried by top 3 Elves with most calories (BIG input)") {
    val input = getLinesFromFile("src/test/scala/adventofcode22/day1_input.txt")
    assertEquals(getTop3ElvesWithMostCaloriesTotalCalories(input), Some(203_905))
  }

}
object Day1Suite {

  def splitListByInputGen(alphabet: NonEmptyList[Char]): Gen[(Char, String)] = {
    val NonEmptyList(c, rest) = alphabet
    val nonEmptyStringFromRestGen = Gen.chooseNum(1, 5).flatMap(n => Gen.stringOfN(n, Gen.oneOf(rest)))
    Gen.zip(
      Gen.const(c),
      Gen.chooseNum(1, 10).flatMap(n => Gen.listOfN(n, nonEmptyStringFromRestGen).map(_.mkString(s"$c")))
    )
  }

}
