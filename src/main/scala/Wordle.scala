import cats.data.NonEmptyList
import Wordle.Char._
import Wordle.GuessResult._
import Wordle.PositionStatus._

object Wordle {

  case class Word[A](v1: A, v2: A, v3: A, v4: A, v5: A) {
    lazy val toNel: NonEmptyList[A] = NonEmptyList.of(v1, v2, v3, v4, v5)
  }

  sealed trait PositionStatus
  object PositionStatus {
    case object NotPresent extends PositionStatus
    case object PresentButIncorrectPosition extends PositionStatus
    case object PresentAndCorrectPosition extends PositionStatus
  }

  case class Solution[A](value: Word[A]) {
    def contains(a: A): Boolean = value.toNel.exists(_ == a)
  }

  case class Guess[A](value: Word[A])

  sealed trait GuessResult
  object GuessResult {
    case object CorrectGuess extends GuessResult
    case object IncorrectGuess extends GuessResult
  }

  case class GuessStatus[A](value: Word[(A, PositionStatus)])

  def getGuessStatus[A](s: Solution[A], g: Guess[A]): GuessStatus[A] = {
    val getStatus: (A, A) => PositionStatus = getPositionStatus(s)
    GuessStatus(
      Word(
        v1 = (g.value.v1, getStatus(s.value.v1, g.value.v1)),
        v2 = (g.value.v2, getStatus(s.value.v2, g.value.v2)),
        v3 = (g.value.v3, getStatus(s.value.v3, g.value.v3)),
        v4 = (g.value.v4, getStatus(s.value.v4, g.value.v4)),
        v5 = (g.value.v5, getStatus(s.value.v5, g.value.v5))
      )
    )
  }

  def getPositionStatus[A](solution: Solution[A])(solutionElem: A, guessElem: A): PositionStatus =
    if (solutionElem == guessElem) PresentAndCorrectPosition
    else if (solution.contains(guessElem)) PresentButIncorrectPosition
    else NotPresent

  sealed trait Char {
    override def toString: String = this match {
      case Char.A => "A"
      case Char.B => "B"
      case Char.C => "C"
      case Char.D => "D"
      case Char.E => "E"
      case Char.F => "F"
      case Char.G => "G"
      case Char.H => "H"
      case Char.I => "I"
      case Char.J => "J"
      case Char.K => "K"
      case Char.L => "L"
      case Char.M => "M"
      case Char.N => "N"
      case Char.O => "O"
      case Char.P => "P"
      case Char.Q => "Q"
      case Char.R => "R"
      case Char.S => "S"
      case Char.T => "T"
      case Char.U => "U"
      case Char.V => "V"
      case Char.W => "W"
      case Char.X => "X"
      case Char.Y => "Y"
      case Char.Z => "Z"
    }
  }
  object Char {
    case object A extends Char
    case object B extends Char
    case object C extends Char
    case object D extends Char
    case object E extends Char
    case object F extends Char
    case object G extends Char
    case object H extends Char
    case object I extends Char
    case object J extends Char
    case object K extends Char
    case object L extends Char
    case object M extends Char
    case object N extends Char
    case object O extends Char
    case object P extends Char
    case object Q extends Char
    case object R extends Char
    case object S extends Char
    case object T extends Char
    case object U extends Char
    case object V extends Char
    case object W extends Char
    case object X extends Char
    case object Y extends Char
    case object Z extends Char
  }

  def getGuessResult[A](guessStatus: GuessStatus[A]): GuessResult =
    if (guessStatus.value.toNel.map { case (_, ps) => ps }.forall(_ == PresentAndCorrectPosition)) CorrectGuess
    else IncorrectGuess

  def main(args: Array[String]): Unit = {
    val s: Solution[Char] = Solution(Word(C, O, D, E, R))
    val g: Guess[Char] = Guess(Word(D, E, C, O, R))
    val gs = getGuessStatus(s, g)
    println(gs)
    val gr = getGuessResult(gs)
    println(gr)
  }

}
