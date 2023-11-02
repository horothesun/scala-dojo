package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._
import ListOps._

object Day1 {

  type NonEmptyString = NonEmptyList[Char]

  case class ElfPack(foodItems: List[FoodItem]) {
    def totalCalories: Int = foodItems.map(_.calories).sum
  }
  object ElfPack {
    def from(items: List[NonEmptyString]): Option[ElfPack] = items.traverse(FoodItem.from).map(ElfPack.apply)
  }

  case class FoodItem(calories: Int)
  object FoodItem {
    def from(s: NonEmptyString): Option[FoodItem] = s.toList.mkString.toIntOption.map(FoodItem.apply)
  }

  def getTop3ElvesWithMostCaloriesTotalCalories(input: List[String]): Option[Int] =
    parseElfPacks(input).map(getTop3ElvesWithMostCaloriesTotalCalories)

  def getTop3ElvesWithMostCaloriesTotalCalories(eps: List[ElfPack]): Int =
    eps.map(_.totalCalories).sorted.reverse.take(3).sum

  def getElfWithMostCaloriesTotalCalories(input: List[String]): Option[Int] =
    parseElfPacks(input).map(getElfWithMostCaloriesTotalCalories)

  def getElfWithMostCaloriesTotalCalories(eps: List[ElfPack]): Int = eps.map(_.totalCalories).max

  def parseElfPacks(input: List[String]): Option[List[ElfPack]] =
    splitListBy[String]("")(input)
      .traverse(_.traverse[Option, NonEmptyString](_.toList.toNel))
      .flatMap(_.traverse(ElfPack.from))

}
