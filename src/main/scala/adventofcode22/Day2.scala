package adventofcode22

import cats.implicits._
import Day2.Shape._
import Day2.Winner._

object Day2 {

  sealed trait Shape {
    val score: Score = Score(this match {
      case Rock     => 1
      case Paper    => 2
      case Scissors => 3
    })
  }
  object Shape {
    case object Rock extends Shape
    case object Paper extends Shape
    case object Scissors extends Shape
  }

  case class Round(me: Shape, opponent: Shape)
  object Round {

    def from(s: String): Option[Round] =
      s.stripMargin.split(' ').map(_.toList) match {
        case Array(o :: Nil, m :: Nil) => (parseMyShape(m), parseOpponentShape(o)).mapN(Round.apply)
        case _                         => None
      }

    def fromPlan(p: RoundPlan): Round = {
      val me = p.desiredWinner match {
        case Me =>
          p.opponent match {
            case Rock     => Paper
            case Paper    => Scissors
            case Scissors => Rock
          }
        case Opponent =>
          p.opponent match {
            case Rock     => Scissors
            case Paper    => Rock
            case Scissors => Paper
          }
        case Draw => p.opponent
      }
      Round(me, opponent = p.opponent)
    }

  }

  sealed trait Winner
  object Winner {
    case object Me extends Winner
    case object Opponent extends Winner
    case object Draw extends Winner
  }

  case class Score(value: Int) {
    def `+`(that: Score): Score = Numeric[Score].plus(this, that)
  }
  object Score {
    implicit val numeric: Numeric[Score] = new Numeric[Score] {
      override def plus(x: Score, y: Score): Score = Score(x.value + y.value)
      override def minus(x: Score, y: Score): Score = Score(x.value - y.value)
      override def times(x: Score, y: Score): Score = Score(x.value * y.value)
      override def negate(x: Score): Score = Score(-x.value)
      override def fromInt(x: Int): Score = Score(x)
      override def parseString(str: String): Option[Score] = str.toIntOption.map(Score.apply)
      override def toInt(x: Score): Int = x.value
      override def toLong(x: Score): Long = x.value.toLong
      override def toFloat(x: Score): Float = x.value.toFloat
      override def toDouble(x: Score): Double = x.value.toDouble
      override def compare(x: Score, y: Score): Int = x.value.compare(y.value)
    }
  }

  case class RoundOutcome(winner: Winner, me: Score, opponent: Score)

  def parseMyShape(c: Char): Option[Shape] =
    c match {
      case 'X' => Some(Rock)
      case 'Y' => Some(Paper)
      case 'Z' => Some(Scissors)
      case _   => None
    }

  def parseOpponentShape(c: Char): Option[Shape] =
    c match {
      case 'A' => Some(Rock)
      case 'B' => Some(Paper)
      case 'C' => Some(Scissors)
      case _   => None
    }

  def parseMatch(input: List[String]): Option[List[Round]] = input.traverse(Round.from)

  def getRoundOutcome(r: Round): RoundOutcome = {
    val (winner, m, o) = (r.me, r.opponent) match {
      case (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => (Me, Score(6), Score(0))
      case (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => (Draw, Score(3), Score(3))
      case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => (Opponent, Score(0), Score(6))
    }
    RoundOutcome(
      winner,
      me = m + r.me.score,
      opponent = o + r.opponent.score
    )
  }

  def getMyTotalScore(input: List[String]): Option[Score] =
    parseMatch(input).map(_.map(getRoundOutcome).map(_.me).sum)

  case class RoundPlan(opponent: Shape, desiredWinner: Winner)
  object RoundPlan {
    def from(s: String): Option[RoundPlan] =
      s.stripMargin.split(' ').map(_.toList) match {
        case Array(o :: Nil, dw :: Nil) => (parseOpponentShape(o), parseDesiredWinner(dw)).mapN(RoundPlan.apply)
        case _                          => None
      }
  }

  def parseDesiredWinner(c: Char): Option[Winner] =
    c match {
      case 'X' => Some(Opponent)
      case 'Y' => Some(Draw)
      case 'Z' => Some(Me)
      case _   => None
    }

  def parsePlannedMatch(input: List[String]): Option[List[RoundPlan]] = input.traverse(RoundPlan.from)

  def getMyPlannedTotalScore(input: List[String]): Option[Score] =
    parsePlannedMatch(input).map(_.map(Round.fromPlan).map(getRoundOutcome).map(_.me).sum)

}
