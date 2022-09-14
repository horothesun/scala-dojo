import scala.collection.immutable.TreeMap

/*
  An arcade game player wants to climb to the top of the leaderboard and track their ranking.
  The game uses Dense Ranking, so its leaderboard works like this:
  - The player with the highest score is ranked number  on the leaderboard.
  - Players who have equal scores receive the same ranking number, and the next player(s) receive the immediately following ranking number.

  Example
    ranked: [100, 90, 90, 80]
    player: [70, 80, 105]

  The ranked players will have ranks 1, 2, 2, and 3, respectively.
  If the player's scores are 70, 80 and 105, their rankings after each game are 4th, 3rd and 1st.
  Return [4, 3, 1].
 */
object ClimbingTheLeaderboard {

  case class Score(value: Int) extends AnyVal
  object Score {
    implicit val ordering: Ordering[Score] = Ordering.by(_.value)
  }

  case class Ranking(value: Int) extends AnyVal

  case class Leaderboard(scores: TreeMap[Score, Unit]) {
    import Leaderboard.InsertionResult

    def insert(score: Score): InsertionResult = {
      val newScores = scores + (score -> ())
      val ranking = Ranking(newScores.size - newScores.keysIterator.indexOf(score))
      InsertionResult(ranking, Leaderboard(newScores))
    }
  }
  object Leaderboard {
    case class InsertionResult(
      ranking: Ranking,
      newLeaderboard: Leaderboard
    )

    def from(ranked: Array[Int]): Leaderboard = Leaderboard(
      scores = ranked.groupBy(Score.apply).map { case (s, _) => (s, ()) }.to(TreeMap)
    )
  }

  def climbingLeaderboard(ranked: Array[Int], player: Array[Int]): Array[Int] = {
    val (rankings, _) = player.foldLeft(
      (Vector.empty[Ranking], Leaderboard.from(ranked))
    ) { case ((rs, l), s) =>
      val ir = l.insert(Score(s))
      (rs :+ ir.ranking, ir.newLeaderboard)
    }
    rankings.map(_.value).toArray
  }

}
