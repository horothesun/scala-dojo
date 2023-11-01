package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._

object Day4 {

  case class Section(id: Int)

  case class SectionRange(value: NonEmptyList[Section]) {
    def isFullyContainedIn(that: SectionRange): Boolean = value.toList.intersect(that.value.toList) == value.toList
    def isOverlapping(that: SectionRange): Boolean = value.toList.intersect(that.value.toList).nonEmpty
  }
  object SectionRange {
    def from(s: String): Option[SectionRange] = s.split('-') match {
      case Array(minS, maxS) =>
        (
          minS.toIntOption,
          maxS.toIntOption
        ).flatMapN { case (min, max) => (min to max).map(Section.apply).toList.toNel }
          .map(SectionRange.apply)
      case _ => None
    }
  }

  case class AssignmentPair(l: SectionRange, r: SectionRange) {
    def isOneRangeFullyContainedIntoTheOther: Boolean = l.isFullyContainedIn(r) || r.isFullyContainedIn(l)
    def areRangesOverlapping: Boolean = l.isOverlapping(r)
  }
  object AssignmentPair {
    def from(s: String): Option[AssignmentPair] =
      s.split(',') match {
        case Array(l, r) => (SectionRange.from(l), SectionRange.from(r)).mapN(AssignmentPair.apply)
        case _           => None
      }
  }

  def parseAssignmentPairs(input: List[String]): Option[List[AssignmentPair]] = input.traverse(AssignmentPair.from)

  def getAssignmentPairsWithFullyContainedRangeCount(input: List[String]): Option[Int] =
    parseAssignmentPairs(input).map(getAssignmentPairsWithFullyContainedRangeCount)

  def getAssignmentPairsWithFullyContainedRangeCount(aps: List[AssignmentPair]): Int =
    aps.count(_.isOneRangeFullyContainedIntoTheOther)

  def getAssignmentPairsWithOverlappingRangesCount(input: List[String]): Option[Int] =
    parseAssignmentPairs(input).map(getAssignmentPairsWithOverlappingRangesCount)

  def getAssignmentPairsWithOverlappingRangesCount(aps: List[AssignmentPair]): Int =
    aps.count(_.areRangesOverlapping)

}
