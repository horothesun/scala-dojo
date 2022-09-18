import ClimbingTheLeaderboard.climbingLeaderboard
import munit.ScalaCheckSuite

class ClimbingTheLeaderboardTest extends ScalaCheckSuite {

  test("climbingLeaderboard on sample test 0") {
    assertEquals(
      climbingLeaderboard(
        ranked = Array(100, 100, 50, 40, 40, 20, 10),
        player = Array(5, 25, 50, 120)
      ).toList,
      List(6, 4, 2, 1)
    )
  }

  test("climbingLeaderboard on sample test 1") {
    assertEquals(
      climbingLeaderboard(
        ranked = Array(100, 90, 90, 80, 75, 60),
        player = Array(50, 65, 77, 90, 102)
      ).toList,
      List(6, 5, 4, 2, 1)
    )
  }

}
