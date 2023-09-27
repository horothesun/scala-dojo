package wordle

import cats._
import cats.data._
import cats.implicits._

import scala.io.Source
import Wordle.Char._
import Wordle.GuesserFilter._
import Wordle.GuesserSort._
import Wordle.GuessResult._
import Wordle.PositionStatus._
import Wordle.WordPos._

import scala.collection.immutable

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
    implicit def order[A: Order]: Order[Word[A]] = Order.fromOrdering[Word[A]]

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
        val showedValue = gs.value.map { case (a, ps) => s"(${Show[A].show(a)}, ${Show[PositionStatus].show(ps)})" }
        s"GuessStatus($showedValue)"
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

  case class Dictionary[A](words: Set[Word[A]])

  case class GuessHistory[A](guessStatuses: List[GuessStatus[A]])
  object GuessHistory {
    implicit def show[A: Show]: Show[GuessHistory[A]] =
      Show.show[GuessHistory[A]](_.guessStatuses.map(_.show).mkString("GuessHistory(\n  ", ",\n  ", "\n)"))

    def empty[A]: GuessHistory[A] = GuessHistory(guessStatuses = List.empty)
  }

  sealed trait Suggester[A] {
    def getSuggestions(d: Dictionary[A]): Set[Word[A]]
  }
  object Suggester {

    case class All[A]() extends Suggester[A] {
      override def getSuggestions(d: Dictionary[A]): Set[Word[A]] = d.words
    }
    case class AbsentFromWord[A](a: A) extends Suggester[A] {
      override def getSuggestions(d: Dictionary[A]): Set[Word[A]] = d.words.filterNot(_.contains(a))
    }
    case class WrongPosition[A](a: A, pos: WordPos) extends Suggester[A] {
      override def getSuggestions(d: Dictionary[A]): Set[Word[A]] =
        d.words.filterNot(w => w.at(pos) == a).filter(w => w.contains(a))
    }
    case class MatchingPosition[A](a: A, pos: WordPos) extends Suggester[A] {
      override def getSuggestions(d: Dictionary[A]): Set[Word[A]] = d.words.filter(w => w.at(pos) == a)
    }
    case class And[A](s1: Suggester[A], s2: Suggester[A]) extends Suggester[A] {
      override def getSuggestions(d: Dictionary[A]): Set[Word[A]] = s2.getSuggestions(Dictionary(s1.getSuggestions(d)))
    }

    def fromHistory[A: Order](h: GuessHistory[A]): Suggester[A] =
      h.guessStatuses match {
        case Nil    => Suggester.All[A]()
        case _ :: _ => h.guessStatuses.map(Suggester.from[A]).reduce(Suggester.And[A])
      }

    def from[A: Order](gs: GuessStatus[A]): Suggester[A] = {
      val Word(t1, t2, t3, t4, t5) = gs.valueWithAllStatuses
      List[((A, PositionStatus, NonEmptyList[PositionStatus]), WordPos)](
        (t1, Pos1),
        (t2, Pos2),
        (t3, Pos3),
        (t4, Pos4),
        (t5, Pos5)
      ).map { case ((a, ps, pss), pos) => getSuggester(a, ps, pss, pos) }.mapFilter {
        case All() => None
        case s @ _ => Some(s)
      }.toNel
        .fold[Suggester[A]](ifEmpty = All()) { case NonEmptyList(sHead, sTail) =>
          sTail.foldLeft[Suggester[A]](sHead) { case (s1, s2) => And(s1, s2) }
        }
    }

    def getSuggester[A](
      a: A,
      ps: PositionStatus,
      allStatuses: NonEmptyList[PositionStatus],
      pos: WordPos
    ): Suggester[A] =
      ps match {
        case PositionStatus.Absent =>
          if (allStatuses.forall(_ == PositionStatus.Absent)) Suggester.AbsentFromWord(a) else Suggester.All()
        case PositionStatus.Incorrect => Suggester.WrongPosition(a, pos)
        case PositionStatus.Correct   => Suggester.MatchingPosition(a, pos)
      }

  }

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

  def getGuess[A: Order](filter: GuesserFilter, sort: GuesserSort)(suggestions: Set[Word[A]]): Option[Word[A]] = {
    val filteredGroups = filter match {
      case Unfiltered => List(suggestions)
      case AllDistinctFirst =>
        val (allDistinct, withDuplicates) = suggestions.partition(_.hasAllDistinct)
        List(allDistinct, withDuplicates)
      case WithDuplicatesFirst =>
        val (allDistinct, withDuplicates) = suggestions.partition(_.hasAllDistinct)
        List(withDuplicates, allDistinct)
    }
    val sortedGroups = sort match {
      case Ascending  => filteredGroups.map(g => g.toList.sorted)
      case Descending => filteredGroups.map(g => g.toList.sorted.reverse)
      case Shuffled   => filteredGroups.map(g => scala.util.Random.shuffle(g.toList))
      case Unsorted   => filteredGroups
    }
    sortedGroups.headOption.flatMap(_.headOption)
  }

  def fetchDictionary: Dictionary[Char] = {
    val src = Source.fromFile("src/main/scala/wordle/5-chars-english-words.txt")
    val ws = src.getLines.toList
      .map(_.toList.traverse(Char.apply))
      .collect { case Some(List(c1, c2, c3, c4, c5)) => Word(c1, c2, c3, c4, c5) }
      .toSet
    src.close()
    Dictionary(ws)
  }

  def getHistory[A: Order](
    dictionary: Dictionary[A],
    guesserFilter: GuesserFilter,
    guesserSort: GuesserSort,
    solution: Solution[A]
  ): GuessHistory[A] =
    GuessHistory(List.unfold[GuessStatus[A], (GuessHistory[A], GuessResult)]((GuessHistory.empty, Unsolved)) {
      case (history, guessResult) =>
        guessResult match {
          case Solved => None
          case Unsolved =>
            history.guessStatuses match {
              case _ :: _ :: _ :: _ :: _ :: _ :: Nil => None
              case _ =>
                val suggestions = Suggester.fromHistory(history).getSuggestions(dictionary)
                suggestions.toList match {
                  case Nil => None
                  case _ =>
                    getGuess[A](guesserFilter, guesserSort)(suggestions)
                      .map(guess => GuessStatus.from(solution, guess))
                      .map(gs => (gs, (GuessHistory(history.guessStatuses :+ gs), GuessResult.from(gs))))
                }
            }
        }

    })

  def main(args: Array[String]): Unit = {
//    val solution = Solution(Word[Char](C, O, D, E, R))
//    val guess = Word[Char](D, E, C, O, R)
//    val guessStatus = GuessStatus.from(solution, guess)
//    println(guessStatus.show)
//    val guessResult = GuessResult.from(guessStatus)
//    println(guessResult)

//    println(fetchDictionary.words.map(_.show).take(5).mkString("Dictionary:\n  ", ",\n  ", ",\n  ..."))

//    val history_THUMP = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Absent), (M, Incorrect), (E, Absent), (R, Absent)),
//        Word((M, Incorrect), (O, Absent), (U, Correct), (N, Absent), (T, Incorrect)),
//        Word((T, Correct), (H, Correct), (U, Correct), (M, Correct), (B, Absent)),
//        Word((T, Correct), (H, Correct), (U, Correct), (M, Correct), (P, Correct))
//      ).map(GuessStatus.apply)
//    )

//    val history_SCAMP = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Incorrect), (M, Incorrect), (E, Absent), (R, Absent)),
//        Word((A, Incorrect), (M, InKrrect), (P, Incorrect), (L, Absent), (Y, Absent)),
//        Word((S, Correct), (W, Absent), (A, Correct), (M, Correct), (P, Correct)),
//        Word((S, Correct), (C, Correct), (A, Correct), (M, Correct), (P, Correct))
//      ).map(GuessStatus.apply)
//    )

//    val history_CONDO = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Absent), (M, Absent), (E, Absent), (R, Absent)),
//        Word((B, Absent), (O, Correct), (N, Correct), (U, Absent), (S, Absent)),
//        Word((T, Absent), (O, Correct), (N, Correct), (I, Absent), (C, Incorrect)),
//        Word((C, Correct), (O, Correct), (N, Correct), (C, Absent), (H, Absent)),
//        Word((C, Correct), (O, Correct), (N, Correct), (D, Correct), (O, Correct))
//      ).map(GuessStatus.apply)
//    )

//    val history = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Absent), (M, Absent), (E, Absent), (R, Absent)),
//        Word((B, Absent), (O, Correct), (N, Correct), (U, Absent), (S, Absent))
//      ).map(GuessStatus.apply)
//    )
//    val dict = fetchDictionary
//    val suggestions = Suggester.fromHistory(history).getSuggestions(dict)
//    println(
//      suggestions
//        .map(_.toString)
//        .mkString(s"Suggestions (${suggestions.size}/${dict.words.size}):\n  ", "\n  ", "")
//    )
//    val guess = getGuess[Char](GuesserFilter.Unfiltered, GuesserSort.Shuffled)(suggestions)
//    println(s"Guess: ${guess.fold(ifEmpty = "<N/D>")(_.toString)}")

    val matchHistory = getHistory[Char](
      dictionary = fetchDictionary,
      GuesserFilter.AllDistinctFirst,
      GuesserSort.Shuffled,
      Solution(Word[Char](B, I, L, L, Y))
    )
    println(matchHistory.show)
  }

}
