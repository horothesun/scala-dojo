package wordle

import cats._
import cats.data._
import cats.implicits._
import scala.io.Source
import Wordle.Char._
import Wordle.GuessResult._
import Wordle.PositionStatus._
import Wordle.WordPos._

object Wordle {

  sealed trait WordPos
  object WordPos {
    case object Pos1 extends WordPos
    case object Pos2 extends WordPos
    case object Pos3 extends WordPos
    case object Pos4 extends WordPos
    case object Pos5 extends WordPos
  }

  case class Word[A](p1: A, p2: A, p3: A, p4: A, p5: A) {

    def at: WordPos => A = {
      case Pos1 => p1
      case Pos2 => p2
      case Pos3 => p3
      case Pos4 => p4
      case Pos5 => p5
    }

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

  case class GuessStatus[A](value: Word[(A, PositionStatus)]) {
    def valueWithAllStatuses(implicit o: Order[A]): Word[(A, PositionStatus, NonEmptyList[PositionStatus])] = {
      val statusesByChar = value.toNel.groupMapNem(_._1)(_._2)
      value.map { case (a, ps) => (a, ps, statusesByChar(a).getOrElse(NonEmptyList.one(ps))) }
    }
  }
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

    def apply(char: scala.Char): Option[Char] = char match {
      case 'a' | 'A' => Some(A)
      case 'b' | 'B' => Some(B)
      case 'c' | 'C' => Some(C)
      case 'd' | 'D' => Some(D)
      case 'e' | 'E' => Some(E)
      case 'f' | 'F' => Some(F)
      case 'g' | 'G' => Some(G)
      case 'h' | 'H' => Some(H)
      case 'i' | 'I' => Some(I)
      case 'j' | 'J' => Some(J)
      case 'k' | 'K' => Some(K)
      case 'l' | 'L' => Some(L)
      case 'm' | 'M' => Some(M)
      case 'n' | 'N' => Some(N)
      case 'o' | 'O' => Some(O)
      case 'p' | 'P' => Some(P)
      case 'q' | 'Q' => Some(Q)
      case 'r' | 'R' => Some(R)
      case 's' | 'S' => Some(S)
      case 't' | 'T' => Some(T)
      case 'u' | 'U' => Some(U)
      case 'v' | 'V' => Some(V)
      case 'w' | 'W' => Some(W)
      case 'x' | 'X' => Some(X)
      case 'y' | 'Y' => Some(Y)
      case 'z' | 'Z' => Some(Z)
      case _         => None
    }
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

    case class Empty[A]() extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] = List.empty[Guess[A]]
    }
    case class Const[A](g: Guess[A]) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] = List(g)
    }
    case class All[A]() extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] = d.ws.map(Guess.apply)
    }
    case class AbsentFromWord[A](a: A) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        d.ws.filterNot(_.contains(a)).map(Guess.apply)
    }
    case class WrongPosition[A](a: A, pos: WordPos) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        d.ws.filterNot(w => w.at(pos) == a).map(Guess.apply)
    }
    case class MatchingPosition[A](a: A, pos: WordPos) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        d.ws.filter(w => w.at(pos) == a).map(Guess.apply)
    }
    case class And[A](g1: Guesser[A], g2: Guesser[A]) extends Guesser[A] {
      override def guesses(d: Dictionary[A], history: GuessHistory[A]): List[Guess[A]] =
        g2.guesses(Dictionary(g1.guesses(d, history).map(_.word)), history)
    }

    def from[A](gs: GuessStatus[A])(implicit o: Order[A]): Guesser[A] = {
      val Word(t1, t2, t3, t4, t5) = gs.valueWithAllStatuses
      List[((A, PositionStatus, NonEmptyList[PositionStatus]), WordPos)](
        (t1, Pos1),
        (t2, Pos2),
        (t3, Pos3),
        (t4, Pos4),
        (t5, Pos5)
      ).map { case ((a, ps, pss), pos) => getGuesser(a, ps, pss, pos) }.mapFilter {
        case All() => None
        case g @ _ => Some(g)
      }.toNel
        .fold[Guesser[A]](ifEmpty = All()) { case NonEmptyList(gHead, gTail) =>
          gTail.foldLeft[Guesser[A]](gHead) { case (g1, g2) => And(g1, g2) }
        }
    }

    def getGuesser[A](
      a: A,
      ps: PositionStatus,
      allStatuses: NonEmptyList[PositionStatus],
      pos: WordPos
    ): Guesser[A] =
      ps match {
        case PositionStatus.Absent =>
          if (allStatuses.forall(_ == PositionStatus.Absent)) Guesser.AbsentFromWord(a) else Guesser.All()
        case PositionStatus.IncorrectPosition => Guesser.WrongPosition(a, pos)
        case PositionStatus.CorrectPosition   => Guesser.MatchingPosition(a, pos)
      }
  }

  def getDictionary: Dictionary[Char] = {
    val src = Source.fromFile("src/main/scala/wordle/5-chars-english-words.txt")
    val ws = src.getLines.toList
      .map(_.toList.traverse(Char.apply))
      .collect { case Some(List(c1, c2, c3, c4, c5)) => Word(c1, c2, c3, c4, c5) }
    src.close()
    Dictionary(ws)
  }

  def main(args: Array[String]): Unit = {
    val s = Solution[Char](Word(C, O, D, E, R))
    val g = Guess[Char](Word(D, E, C, O, R))
    val gs = getGuessStatus(s, g)
    println(Show[GuessStatus[Char]].show(gs))
    val gr = getGuessResult(gs)
    println(gr)
    println(getDictionary.ws.take(5).mkString("Dictionary:\n  ", ",\n  ", ",\n  ..."))
  }

}
