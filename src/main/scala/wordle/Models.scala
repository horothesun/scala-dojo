package wordle

import Models.GuessResult._
import Models.PositionStatus._
import Models.WordPos._
import cats._
import cats.data._
import cats.syntax.all._

object Models {

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

    def hasAllDistinct(implicit o: Order[A]): Boolean = occurrences.keys.size == 5

    def map[B](f: A => B): Word[B] = Functor[Word].map(this)(f)

    def pairWith[B](wb: Word[B]): Word[(A, B)] = Word((p1, wb.p1), (p2, wb.p2), (p3, wb.p3), (p4, wb.p4), (p5, wb.p5))

    override def toString: String = s"$p1$p2$p3$p4$p5"

  }
  object Word {
    implicit def eq[A: Eq]: Eq[Word[A]] = Eq.fromUniversalEquals

    implicit val functor: Functor[Word] = new Functor[Word] {
      override def map[A, B](fa: Word[A])(f: A => B): Word[B] = Word(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4), f(fa.p5))
    }

    implicit def ordering[A: Ordering]: Ordering[Word[A]] = Ordering[String].on[Word[A]](_.toString)
    implicit def order[A: Order]: Order[Word[A]] = Order[String].contramap[Word[A]](_.toString)

    implicit def show[A: Show]: Show[Word[A]] =
      Show.show[Word[A]](_.map(Show[A].show).toNel.mkString_("Word(", ",", ")"))
  }

  sealed trait PositionStatus
  object PositionStatus {
    case object Absent extends PositionStatus
    case object Incorrect extends PositionStatus
    case object Correct extends PositionStatus

    implicit val show: Show[PositionStatus] = Show.show[PositionStatus] {
      case Absent    => "⬛️"
      case Incorrect => "🟨"
      case Correct   => "🟩"
    }
  }

  case class Dictionary[A](words: Set[Word[A]])

  case class Solution[A](word: Word[A])

  sealed trait GuessResult {
    override def toString: String = "GuessResult." +
      (this match {
        case Solved   => "Solved"
        case Unsolved => "Unsolved"
      })
  }
  object GuessResult {
    case object Solved extends GuessResult
    case object Unsolved extends GuessResult

    implicit val show: Show[GuessResult] = Show.show[GuessResult] {
      case Solved   => "✅"
      case Unsolved => "❌"
    }

    def from[A](guessStatus: GuessStatus[A]): GuessResult =
      if (guessStatus.value.toNel.map { case (_, ps) => ps }.forall(_ == Correct)) Solved
      else Unsolved
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
        val showedValue = gs.value.map { case (a, ps) => s"(${a.show},${ps.show})" }
        s"GuessStatus(${showedValue.toString})"
      }

    def from[A: Order](s: Solution[A], guess: Word[A]): GuessStatus[A] = {
      val solutionOccurrences = s.word.occurrences
      val exactMatchOccurrences = s.word
        .pairWith(guess)
        .map { case (sa, ga) => if (sa == ga) Some(ga) else None }
        .toNel
        .collect { case Some(a) => a }
        .groupMapReduce(identity)(_ => 1)(_ + _)
      def getPosStatusAt(pos: WordPos): PositionStatus =
        getPositionStatus(solutionOccurrences, exactMatchOccurrences)(s.word.at(pos), guess.at(pos))
      GuessStatus(guess.pairWith(Word(Pos1, Pos2, Pos3, Pos4, Pos5).map(getPosStatusAt)))
    }

    def getPositionStatus[A: Order](
      solutionOccurrences: NonEmptyMap[A, Int],
      exactMatchOccurrences: Map[A, Int]
    )(solutionElem: A, guessElem: A): PositionStatus =
      if (solutionElem == guessElem) Correct
      else
        solutionOccurrences(guessElem).fold[PositionStatus](ifEmpty = Absent) { sgo =>
          exactMatchOccurrences
            .get(guessElem)
            .map(emo => (sgo, emo))
            .fold[PositionStatus](ifEmpty = Incorrect) {
              case (sgo, emo) if sgo == emo => Absent
              case _                        => Incorrect
            }
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

  case class GuessHistory[A](guessStatuses: List[GuessStatus[A]])
  object GuessHistory {
    implicit def show[A: Show]: Show[GuessHistory[A]] =
      Show.show[GuessHistory[A]](_.guessStatuses.map(_.show).mkString("GuessHistory(\n  ", ",\n  ", "\n)"))

    def empty[A]: GuessHistory[A] = GuessHistory(guessStatuses = List.empty)
  }

  case class GuesserConfig(filter: GuesserFilter, sort: GuesserSort)

  sealed trait GuesserFilter
  object GuesserFilter {
    case object Unfiltered extends GuesserFilter
    case object AllDistinctFirst extends GuesserFilter
    case object WithDuplicatesFirst extends GuesserFilter
  }

  sealed trait GuesserSort
  object GuesserSort {
    case object Ascending extends GuesserSort
    case object Descending extends GuesserSort
    case object Shuffled extends GuesserSort
    case object Unsorted extends GuesserSort
  }

}
