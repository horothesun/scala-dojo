import cats._
import cats.data._
import cats.implicits._
import Wordle.Char._
import Wordle.GuessResult._
import Wordle.PositionStatus._

object Wordle {

  case class Word[A](p1: A, p2: A, p3: A, p4: A, p5: A) {
    lazy val toNel: NonEmptyList[A] = NonEmptyList.of(p1, p2, p3, p4, p5)

    def contains(a: A): Boolean = toNel.exists(_ == a)

    def occurrences(implicit o: Order[A]): NonEmptyMap[A, Int] = toNel.groupMapReduceWithNem(identity)(_ => 1)(_ + _)

    def map[B](f: A => B): Word[B] = Functor[Word].map(this)(f)

    def pairWith[B](wb: Word[B]): Word[(A, B)] = Word((p1, wb.p1), (p2, wb.p2), (p3, wb.p3), (p4, wb.p4), (p5, wb.p5))
  }
  object Word {
    implicit def eq[A: Eq]: Eq[Word[A]] = Eq.fromUniversalEquals

    implicit val functor: Functor[Word] = new Functor[Word] {
      override def map[A, B](fa: Word[A])(f: A => B): Word[B] = Word(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4), f(fa.p5))
    }

    implicit def show[A: Show]: Show[Word[A]] = Show.show[Word[A]](_.map(Show[A].show).toString)
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

  case class Solution[A](word: Word[A])

  case class Guess[A](word: Word[A])

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

  def getGuessStatus[A: Order](s: Solution[A], g: Guess[A]): GuessStatus[A] = {
    val solutionOccurrences = s.word.occurrences
    val exactMatchOccurrences = s.word
      .pairWith(g.word)
      .map { case (sa, ga) => if (sa == ga) Some(ga) else None }
      .toNel
      .collect { case Some(a) => a }
      .groupMapReduce(identity)(_ => 1)(_ + _)
    def getPosStatusAt(atPos: Word[A] => A): PositionStatus =
      getPositionStatus(solutionOccurrences, exactMatchOccurrences)(atPos(s.word), atPos(g.word))
    GuessStatus(
      g.word.pairWith(
        Word(
          getPosStatusAt(_.p1),
          getPosStatusAt(_.p2),
          getPosStatusAt(_.p3),
          getPosStatusAt(_.p4),
          getPosStatusAt(_.p5)
        )
      )
    )
  }

  def getPositionStatus[A: Order](
    solutionOccurrences: NonEmptyMap[A, Int],
    exactMatchOccurrences: Map[A, Int]
  )(solutionElem: A, guessElem: A): PositionStatus =
    if (solutionElem == guessElem) CorrectPosition
    else
      solutionOccurrences(guessElem).fold[PositionStatus](ifEmpty = Absent) { sgo =>
        exactMatchOccurrences
          .get(guessElem)
          .map(emo => (sgo, emo))
          .fold[PositionStatus](ifEmpty = IncorrectPosition) {
            case (sgo, emo) if sgo == emo => Absent
            case _                        => IncorrectPosition
          }
      }

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
    implicit val ordering: Ordering[Char] = Ordering[String].on[Char](_.toString)
    implicit val order: Order[Char] = Order.fromOrdering[Char]
  }

  def getGuessResult[A](guessStatus: GuessStatus[A]): GuessResult =
    if (guessStatus.value.toNel.map { case (_, ps) => ps }.forall(_ == CorrectPosition)) Correct
    else Incorrect

  case class Dictionary[A](ws: List[Word[A]])
  case class GuessHistory[A](gss: Set[GuessStatus[A]])

  sealed trait Guesser[A] {
    def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]]
  }
  object Guesser {
//    def const[A](w: Word[A]): Guesser[A] = new Guesser[A] {
//      override def guesses(d: Dictionary[A], guessHistory: GuessHistory[A]): List[Guess[A]] = List(Guess(w))
//    }

    case class Empty[A]() extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] = List.empty[Guess[A]]
    }
    case class Const[A](g: Guess[A]) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] = List(g)
    }
    case class Absent[A](a: A) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        d.ws.filterNot(_.contains(a)).map(Guess.apply)
    }
    case class IncorrectPosition[A](a: A, notAtPos: Word[A] => A) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        d.ws.filterNot(w => notAtPos(w) == a).map(Guess.apply)
    }
    case class CorrectPosition[A](a: A, pos: Word[A] => A) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        d.ws.filter(w => pos(w) == a).map(Guess.apply)
    }
    case class And[A](g1: Guesser[A], g2: Guesser[A]) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        g2.guesses(Dictionary(g1.guesses(d, history).map(_.word)), history)
    }

    def from[A](gs: GuessStatus[A]): Guesser[A] = {
      val Word((a1, ps1), (a2, ps2), (a3, ps3), (a4, ps4), (a5, ps5)) = gs.value
      And(
        getGuesser(a1, ps1, _.p1),
        And(
          getGuesser(a2, ps2, _.p2),
          And(
            getGuesser(a3, ps3, _.p3),
            And(
              getGuesser(a4, ps4, _.p4),
              getGuesser(a5, ps5, _.p5)
            )
          )
        )
      )
    }

    def getGuesser[A](a: A, ps: PositionStatus, pos: Word[A] => A): Guesser[A] =
      ps match {
        case PositionStatus.Absent            => Guesser.Absent(a)
        case PositionStatus.IncorrectPosition => Guesser.IncorrectPosition(a, notAtPos = pos)
        case PositionStatus.CorrectPosition   => Guesser.CorrectPosition(a, pos)
      }
  }

  def main(args: Array[String]): Unit = {
    val s = Solution[Char](Word(C, O, D, E, R))
    val g = Guess[Char](Word(D, E, C, O, R))
    val gs = getGuessStatus(s, g)
    println(Show[GuessStatus[Char]].show(gs))
    val gr = getGuessResult(gs)
    println(gr)
  }

}
