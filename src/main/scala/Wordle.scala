import cats.data.NonEmptyList
import cats.Show
import Wordle.Char._
import Wordle.GuessResult._
import Wordle.PositionStatus._

object Wordle {

  case class Word[A](v1: A, v2: A, v3: A, v4: A, v5: A) {
    lazy val toNel: NonEmptyList[A] = NonEmptyList.of(v1, v2, v3, v4, v5)

    def map[B](f: A => B): Word[B] = Word(v1 = f(v1), v2 = f(v2), v3 = f(v3), v4 = f(v4), v5 = f(v5))
  }
  object Word {
    implicit def show[A: Show]: Show[Word[A]] = Show.show[Word[A]] { w =>
      val showedWord = w.map(Show[A].show)
      s"Word(${showedWord.v1}, ${showedWord.v2}, ${showedWord.v3}, ${showedWord.v4}, ${showedWord.v5})"
    }
  }

  sealed trait PositionStatus
  object PositionStatus {
    case object Absent extends PositionStatus
    case object IncorrectPosition extends PositionStatus
    case object CorrectPosition extends PositionStatus

    implicit val show: Show[PositionStatus] = Show.show[PositionStatus] {
      case Absent            => "⬛️"
      case IncorrectPosition => "🟨"
      case CorrectPosition   => "🟩"
    }
  }

  case class Solution[A](value: Word[A]) {
    def contains(a: A): Boolean = value.toNel.exists(_ == a)
  }

  case class Guess[A](value: Word[A])

  sealed trait GuessResult {
    override def toString: String = "GuessResult." +
      (this match {
        case Correct   => "Correct"
        case Incorrect => "Incorrect"
      })
  }
  object GuessResult {
    case object Correct extends GuessResult
    case object Incorrect extends GuessResult

    implicit val show: Show[GuessResult] = Show.show[GuessResult] {
      case Correct   => "✅"
      case Incorrect => "❌"
    }
  }

  case class GuessStatus[A](value: Word[(A, PositionStatus)])
  object GuessStatus {
    implicit def show[A: Show]: Show[GuessStatus[A]] =
      Show.show[GuessStatus[A]] { gs =>
        val showedValue = gs.value.map { case (a, ps) => s"(${Show[A].show(a)}, ${Show[PositionStatus].show(ps)})" }
        s"GuessStatus($showedValue)"
      }
  }

  def getGuessStatus[A](s: Solution[A], g: Guess[A]): GuessStatus[A] = {
    val getStatus = getPositionStatus(s) _
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
    if (solutionElem == guessElem) CorrectPosition
    else if (solution.contains(guessElem)) IncorrectPosition
    else Absent

  sealed trait Char
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

    implicit val show: Show[Char] = Show.show[Char] {
      case A => "A"
      case B => "B"
      case C => "C"
      case D => "D"
      case E => "E"
      case F => "F"
      case G => "G"
      case H => "H"
      case I => "I"
      case J => "J"
      case K => "K"
      case L => "L"
      case M => "M"
      case N => "N"
      case O => "O"
      case P => "P"
      case Q => "Q"
      case R => "R"
      case S => "S"
      case T => "T"
      case U => "U"
      case V => "V"
      case W => "W"
      case X => "X"
      case Y => "Y"
      case Z => "Z"
    }
  }

  def getGuessResult[A](guessStatus: GuessStatus[A]): GuessResult =
    if (guessStatus.value.toNel.map { case (_, ps) => ps }.forall(_ == CorrectPosition)) Correct
    else Incorrect

  def main(args: Array[String]): Unit = {
    val s = Solution[Char](Word(C, O, D, E, R))
    val g = Guess[Char](Word(D, E, C, O, R))
    val gs = getGuessStatus(s, g)
    println(Show[GuessStatus[Char]].show(gs))
    val gr = getGuessResult(gs)
    println(gr)
  }

}
