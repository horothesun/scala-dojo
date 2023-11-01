package adventofcode22

import munit.ScalaCheckSuite
import Day2._
import Day2.Shape._
import Day2.Winner._
import FileLoader._

class Day2Suite extends ScalaCheckSuite {

  test("Round.from(\"A Y\") returns Some(Round(me = Paper, opponent = Rock))") {
    assertEquals(Round.from("A Y"), Some(Round(me = Paper, opponent = Rock)))
  }

  test("getRoundOutcome(Round(me = Paper, opponent = Rock)) returns RoundOutcome(Me, Score(8), Score(1))") {
    assertEquals(
      getRoundOutcome(Round(me = Paper, opponent = Rock)),
      RoundOutcome(winner = Me, me = Score(8), opponent = Score(1))
    )
  }

  test("getMyTotalScore from BIG input") {
    val input = getLinesFromFile("src/test/scala/adventofcode22/day2_input.txt")
    assertEquals(getMyTotalScore(input), Some(Score(12_586)))
  }

  test("getMyPlannedTotalScore from BIG input") {
    val input = getLinesFromFile("src/test/scala/adventofcode22/day2_input.txt")
    assertEquals(getMyPlannedTotalScore(input), Some(Score(13_193)))
  }

}
