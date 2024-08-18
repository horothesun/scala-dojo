package adventofcode22

import cats.data.NonEmptySet
import cats.syntax.all._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import Day6._
import Day6Suite._

class Day6Suite extends ScalaCheckSuite {

  property("areAllDifferent on unique non-empty list is always true") {
    forAll(nonEmptySetGen)(nes => assert(areAllDifferent(nes.toNonEmptyList)))
  }

  test("getPrefixAndFinal0BasedIndex") {
    assertEquals(
      getPrefixAndFinal0BasedIndex(maxPrefixLength = 3, List('a', 'b', 'a', 'd', 'e')),
      Some((List('b', 'a', 'd'), 3))
    )
  }

  test("getFinal1BasedIndex(maxPrefixLength = 4, \"mjqjpqmgbljsphdztnvjfqwrcgsmlb\".toList) returns Some(7)") {
    val input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 4, input), Some(7))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 4, \"bvwbjplbgvbhsrlpgdmjqwftvncz\".toList) returns Some(5)") {
    val input = "bvwbjplbgvbhsrlpgdmjqwftvncz".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 4, input), Some(5))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 4, \"nppdvjthqldpwncqszvftbrmjlhg\".toList) returns Some(5)") {
    val input = "nppdvjthqldpwncqszvftbrmjlhg".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 4, input), Some(6))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 4, \"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg\".toList) returns Some(10)") {
    val input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 4, input), Some(10))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 4, \"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw\".toList) returns Some(11)") {
    val input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 4, input), Some(11))
  }

  test("getStartOfPacketMarkerIndex(bigInput) returns valid value") {
    assertEquals(getStartOfPacketMarkerIndex(bigInput), Some(1850))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 14, \"mjqjpqmgbljsphdztnvjfqwrcgsmlb\".toList) returns Some(19)") {
    val input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 14, input), Some(19))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 14, \"bvwbjplbgvbhsrlpgdmjqwftvncz\".toList) returns Some(23)") {
    val input = "bvwbjplbgvbhsrlpgdmjqwftvncz".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 14, input), Some(23))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 14, \"nppdvjthqldpwncqszvftbrmjlhg\".toList) returns Some(23)") {
    val input = "nppdvjthqldpwncqszvftbrmjlhg".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 14, input), Some(23))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 14, \"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg\".toList) returns Some(29)") {
    val input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 14, input), Some(29))
  }

  test("getFinal1BasedIndex(maxPrefixLength = 14, \"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw\".toList) returns Some(26)") {
    val input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toList
    assertEquals(getFinal1BasedIndex(maxPrefixLength = 14, input), Some(26))
  }

  test("getStartOfMessageMarkerIndex(bigInput) returns valid value") {
    assertEquals(getStartOfMessageMarkerIndex(bigInput), Some(2823))
  }

}
object Day6Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day6_input.txt")

  val nonEmptySetGen: Gen[NonEmptySet[Int]] =
    Gen
      .zip(
        Gen.posNum[Int],
        Gen.listOf(Gen.posNum[Int])
      )
      .map { case (i, is) => NonEmptySet.of(i, is: _*) }

}
