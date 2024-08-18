package adventofcode22

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.Order

object Day6 {

  def areAllDifferent[A: Order](as: NonEmptyList[A]): Boolean = as.toNes.size == as.length

  def getPrefixAndFinal0BasedIndex[A: Order](maxPrefixLength: Int, as: List[A]): Option[(List[A], Int)] =
    as.zipWithIndex
      .scanLeft[(List[A], Int)]((List.empty, -1)) { case ((acc, _), (c, i)) =>
        ((acc :+ c).takeRight(maxPrefixLength), i)
      }
      .find { case (prefixSoFar, _) =>
        prefixSoFar.length == maxPrefixLength &&
        prefixSoFar.toNel.exists(areAllDifferent)
      }

  def getFinal1BasedIndex[A: Order](maxPrefixLength: Int, as: List[A]): Option[Int] =
    getPrefixAndFinal0BasedIndex(maxPrefixLength, as).map { case (_, i) => i + 1 }

  def parseChars(input: List[String]): Option[List[Char]] =
    input match {
      case firstLine :: Nil => Some(firstLine.toList)
      case _                => None
    }

  def getStartOfPacketMarkerIndex(input: List[String]): Option[Int] =
    parseChars(input).flatMap(cs => getFinal1BasedIndex(maxPrefixLength = 4, cs))

  def getStartOfMessageMarkerIndex(input: List[String]): Option[Int] =
    parseChars(input).flatMap(cs => getFinal1BasedIndex(maxPrefixLength = 14, cs))

}
