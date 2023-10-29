package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._

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
    parseElfPacks(input).map(_.map(_.totalCalories).sorted.reverse.take(3).sum)

  def getElfWithMostCaloriesTotalCalories(input: List[String]): Option[Int] =
    parseElfPacks(input).map(_.map(_.totalCalories).max)

  def parseElfPacks(input: List[String]): Option[List[ElfPack]] =
    splitListBy[String]("")(input)
      .traverse(_.traverse[Option, NonEmptyString](_.toList.toNel))
      .flatMap(_.traverse(ElfPack.from))

  def splitListBy[A](a: A)(as: List[A]): List[List[A]] =
    (as :+ a)
      .foldLeft((List.empty[List[A]], List.empty[A])) { case ((res, acc), next) =>
        if (next == a) (res :+ acc, List.empty) else (res, acc :+ next)
      }
      ._1

  def getLinesFromFile(filename: String): List[String] = {
    val source = scala.io.Source.fromFile(filename)
    val result = source.getLines.toList
    source.close
    result
  }

}
