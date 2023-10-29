package adventofcode22

import munit.ScalaCheckSuite
import Day1._

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

  test("total Calories carried by that Elf with most calories (small input)") {
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

  test("total Calories carried by that Elf with most calories (BIG input)") {
    val input = getLinesFromFile("src/test/scala/adventofcode22/day1_part1_input.txt")
    assertEquals(getElfWithMostCaloriesTotalCalories(input), Some(70_764))
  }

}
