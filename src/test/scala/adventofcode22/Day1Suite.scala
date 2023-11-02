package adventofcode22

import munit.ScalaCheckSuite
import Day1._
import FileLoader._

class Day1Suite extends ScalaCheckSuite {

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
        |""".stripMargin.linesIterator.toList.drop(1)
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
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getTop3ElvesWithMostCaloriesTotalCalories(input), Some(45_000))
  }

  test("total Calories carried by top 3 Elves with most calories (BIG input)") {
    val input = getLinesFromFile("src/test/scala/adventofcode22/day1_input.txt")
    assertEquals(getTop3ElvesWithMostCaloriesTotalCalories(input), Some(203_905))
  }

}
