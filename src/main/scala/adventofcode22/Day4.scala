package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._

object Day4 {

  case class Section(id: Int)

  case class SectionRange(value: NonEmptyList[Section]) {
    def isFullyContainedIn(that: SectionRange): Boolean = value.toList.intersect(that.value.toList) == value.toList
    def isOverlapping(that: SectionRange): Boolean = value.toList.intersect(that.value.toList).nonEmpty
  }

  case class AssignmentPair(lsr: SectionRange, rsr: SectionRange) {
    def isOneRangeFullyContainedIntoTheOther: Boolean = lsr.isFullyContainedIn(rsr) || rsr.isFullyContainedIn(lsr)
    def areRangesOverlapping: Boolean = lsr.isOverlapping(rsr)
  }

  def parseSectionRange(s: String): Option[SectionRange] = s.split('-') match {
    case Array(minS, maxS) =>
      (
        minS.toIntOption,
        maxS.toIntOption
      ).mapN { case (min, max) => Range.inclusive(min, max).toList.map(Section.apply) }
        .flatMap(_.toNel)
        .map(SectionRange.apply)
    case _ => None
  }

  def parseSectionRangePair(s: String): Option[AssignmentPair] =
    s.split(',') match {
      case Array(l, r) => (parseSectionRange(l), parseSectionRange(r)).mapN(AssignmentPair.apply)
      case _           => None
    }

  def parseAssignmentPairs(input: List[String]): Option[List[AssignmentPair]] =
    input.traverse(parseSectionRangePair)

  def getAssignmentPairsWithFullyContainedRangeCount(input: List[String]): Option[Int] =
    parseAssignmentPairs(input).map(getAssignmentPairsWithFullyContainedRangeCount)

  def getAssignmentPairsWithFullyContainedRangeCount(aps: List[AssignmentPair]): Int =
    aps.count(_.isOneRangeFullyContainedIntoTheOther)

  def getAssignmentPairsWithOverlappingRangesCount(input: List[String]): Option[Int] =
    parseAssignmentPairs(input).map(getAssignmentPairsWithOverlappingRangesCount)

  def getAssignmentPairsWithOverlappingRangesCount(aps: List[AssignmentPair]): Int =
    aps.count(_.areRangesOverlapping)

}
