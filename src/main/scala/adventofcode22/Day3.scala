package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._
import Day3.Item._

object Day3 {

  case class Item(c: Char) {
    def priority: Int =
      if (LOWERCASE_CHARS.contains(c)) c.toInt - 96
      else if (UPPERCASE_CHARS.contains(c)) c.toInt - 38
      else 0
  }
  object Item {
    private val LOWERCASE_CHARS = 'a' to 'z'
    private val UPPERCASE_CHARS = 'A' to 'Z'

    def from(c: Char): Option[Item] =
      if ((LOWERCASE_CHARS ++ UPPERCASE_CHARS).contains(c)) Some(Item(c))
      else None
  }

  case class Rucksack(firstHalf: NonEmptyList[Item], secondHalf: NonEmptyList[Item]) {

    def repeatedItemsBetweenCompartments: List[Item] = firstHalf.toList.intersect(secondHalf.toList)

    def allItems: NonEmptyList[Item] = firstHalf.concatNel(secondHalf)

  }
  object Rucksack {
    def from(s: String): Option[Rucksack] =
      if (s.length % 2 != 0) None
      else {
        val (l, r) = s.splitAt(s.length / 2)
        (l.toList, r.toList) match {
          case (lh :: lt, rh :: rt) =>
            (
              NonEmptyList(lh, lt).traverse(Item.from),
              NonEmptyList(rh, rt).traverse(Item.from)
            ).mapN(Rucksack.apply)
          case _ => None
        }
      }
  }

  def parseRucksacks(input: List[String]): Option[List[Rucksack]] = input.traverse(Rucksack.from)

  def getRepeatedItemsPrioritiesSum(input: List[String]): Option[Int] =
    parseRucksacks(input).map(getRepeatedItemsPrioritiesSum)

  def getRepeatedItemsPrioritiesSum(rs: List[Rucksack]): Int =
    rs.mapFilter(_.repeatedItemsBetweenCompartments.headOption).map(_.priority).sum

  def getGroupedBadgeItemsPrioritiesSum(input: List[String]): Option[Int] =
    parseRucksacks(input).map(getGroupedBadgeItemsPrioritiesSum)

  def getGroupedBadgeItemsPrioritiesSum(rs: List[Rucksack]): Int =
    rs.grouped(3).toList.mapFilter(g => getItemsPresentInAllRucksacks(g).headOption).map(_.priority).sum

  def getItemsPresentInAllRucksacks(rs: List[Rucksack]): List[Item] = rs.map(_.allItems.toList).reduce(_ intersect _)

}
