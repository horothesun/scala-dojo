package wordle

import cats._
import cats.data._
import cats.implicits._
import Models._
import Models.WordPos._

sealed trait Suggester[A] {
  def getSuggestions(d: Dictionary[A]): Set[Word[A]]

  def and(that: Suggester[A]): Suggester[A] = Suggester.And[A](this, that)
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
      case _ :: _ => h.guessStatuses.map(Suggester.from[A]).reduce(_ and _)
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
        sTail.foldLeft[Suggester[A]](sHead)(_ and _)
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
