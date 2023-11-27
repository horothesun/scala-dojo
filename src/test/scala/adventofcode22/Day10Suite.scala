package adventofcode22

import munit.ScalaCheckSuite
import Day10._
import Day10Suite._

class Day10Suite extends ScalaCheckSuite {

  test("getSignalStrengthAtCyclesOfInterestSum(bigInput) is Some(SignalStrength(17_020))") {
    assertEquals(getSignalStrengthAtCyclesOfInterestSum(bigInput), Some(SignalStrength(17_020)))
  }

}
object Day10Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day10_input.txt")

}
