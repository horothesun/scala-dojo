package adventofcode22

import Day4._
import Day4Suite._
import cats.data.NonEmptyList
import munit.ScalaCheckSuite

class Day4Suite extends ScalaCheckSuite {

  test("SectionRange.from(\"3-5\") returns correct section list") {
    assertEquals(
      SectionRange.from("3-5"),
      Some(SectionRange(NonEmptyList.of(Section(3), Section(4), Section(5))))
    )
  }

  test("AssignmentPair.from(\"2-3,5-6\") returns correct section list pair") {
    assertEquals(
      AssignmentPair.from("2-3,5-6"),
      Some(
        AssignmentPair(
          SectionRange(NonEmptyList.of(Section(2), Section(3))),
          SectionRange(NonEmptyList.of(Section(5), Section(6)))
        )
      )
    )
  }

  test("\"4-5\" is fully contained in \"4-6\"") {
    assert(
      SectionRange(NonEmptyList.of(Section(4), Section(5)))
        .isFullyContainedIn(SectionRange(NonEmptyList.of(Section(4), Section(5), Section(6))))
    )
  }

  test("\"5-5\" is fully contained in \"4-6\"") {
    assert(
      SectionRange(NonEmptyList.of(Section(5)))
        .isFullyContainedIn(SectionRange(NonEmptyList.of(Section(4), Section(5), Section(6))))
    )
  }

  test("\"6-6\" is fully contained in \"4-6\"") {
    assert(
      SectionRange(NonEmptyList.of(Section(6)))
        .isFullyContainedIn(SectionRange(NonEmptyList.of(Section(4), Section(5), Section(6))))
    )
  }

  test("\"4-6\" is NOT fully contained in \"6-6\"") {
    assert(
      !SectionRange(NonEmptyList.of(Section(4), Section(5), Section(6)))
        .isFullyContainedIn(SectionRange(NonEmptyList.of(Section(6))))
    )
  }

  test("\"4-6\" is NOT fully contained in \"8-9\"") {
    assert(
      !SectionRange(NonEmptyList.of(Section(4), Section(5), Section(6)))
        .isFullyContainedIn(SectionRange(NonEmptyList.of(Section(8), Section(9))))
    )
  }

  test("\"8-9\" is NOT fully contained in \"4-6\"") {
    assert(
      !SectionRange(NonEmptyList.of(Section(8), Section(9)))
        .isFullyContainedIn(SectionRange(NonEmptyList.of(Section(4), Section(5), Section(6))))
    )
  }

  test("getAssignmentPairsWithFullyContainedRangeCount (small input)") {
    val input =
      """
        |2-4,6-8
        |2-3,4-5
        |5-7,7-9
        |2-8,3-7
        |6-6,4-6
        |2-6,4-8
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getAssignmentPairsWithFullyContainedRangeCount(input), Some(2))
  }

  test("getAssignmentPairsWithFullyContainedRangeCount(bigInput) returns valid value") {
    assertEquals(getAssignmentPairsWithFullyContainedRangeCount(bigInput), Some(532))
  }

  test("getAssignmentPairsWithOverlappingRangesCount (small input)") {
    val input =
      """
        |2-4,6-8
        |2-3,4-5
        |5-7,7-9
        |2-8,3-7
        |6-6,4-6
        |2-6,4-8
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getAssignmentPairsWithOverlappingRangesCount(input), Some(4))
  }

  test("getAssignmentPairsWithOverlappingRangesCount(bigInput) returns valid value") {
    assertEquals(getAssignmentPairsWithOverlappingRangesCount(bigInput), Some(854))
  }

}
object Day4Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day4_input.txt")

}
