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

  case class Score(value: Int)
  object Score {
    implicit val ordering: Ordering[Score] = Ordering.by(_.value)
  }

  case class Ranking(value: Int)

  case class ScoreOccurrences(value: Int) {
    def incremented: ScoreOccurrences = ScoreOccurrences(1 + value)
  }

  case class InsertionResult(
    ranking: Ranking,
    newLeaderboard: Leaderboard
  )

  case class Leaderboard(scores: TreeMap[Score, ScoreOccurrences]) {
    def insert(score: Score): InsertionResult = {
      val newScores = scores
        .get(score)
        .fold(scores + (score -> ScoreOccurrences(1)))(occurrences => scores + (score -> occurrences.incremented))
      val ranking = Ranking(newScores.size - newScores.keysIterator.indexOf(score))
      InsertionResult(ranking, Leaderboard(newScores))
    }
  }
  object Leaderboard {
    def from(ranked: Array[Int]): Leaderboard = {
      val scores = TreeMap[Score, ScoreOccurrences]() ++
        ranked.toList.groupMap(Score.apply)(identity).map { case (k, v) => (k, ScoreOccurrences(v.size)) }
      Leaderboard(scores)
    }
  }

  def climbingLeaderboard(ranked: Array[Int], player: Array[Int]): Array[Int] = {
    val leaderboard = Leaderboard.from(ranked)
    val playerScores = player.toList.map(Score.apply)
    val (rankings, _) = playerScores.foldLeft((List.empty[Ranking], leaderboard)) { case ((rs, l), s) =>
      val ir = l.insert(s)
      (rs :+ ir.ranking, ir.newLeaderboard)
    }
    rankings.map(_.value).toArray
  }

}
