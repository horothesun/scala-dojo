package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._
import munit.ScalaCheckSuite
import Day3._
import Day3Suite._

class Day3Suite extends ScalaCheckSuite {

  test("Item('a') to Item('z') priorities are (1 to 26)") {
    val items = ('a' to 'z').toList.traverse(Item.from)
    assertEquals(items.map(_.map(_.priority)), Some((1 to 26).toList))
  }

  test("Item('A') to Item('Z') priorities are (27 to 52)") {
    val items = ('A' to 'Z').toList.traverse(Item.from)
    assertEquals(items.map(_.map(_.priority)), Some((27 to 52).toList))
  }

  test("Rucksack.from(\"\") returns None") {
    assertEquals(Rucksack.from(""), None)
  }

  test("Rucksack.from(\"aBC\") returns None") {
    assertEquals(Rucksack.from("aBC"), None)
  }

  test("Rucksack.from(\"abcDEF\") returns a valid Rucksack") {
    assertEquals(
      Rucksack.from("abcDEF"),
      Some(
        Rucksack(
          firstHalf = NonEmptyList.of(Item('a'), Item('b'), Item('c')),
          secondHalf = NonEmptyList.of(Item('D'), Item('E'), Item('F'))
        )
      )
    )
  }

  test("Rucksack repeatedItemsBetweenCompartments with NO repeated") {
    assertEquals(
      Rucksack(
        firstHalf = NonEmptyList.of(Item('a'), Item('b'), Item('c')),
        secondHalf = NonEmptyList.of(Item('D'), Item('A'), Item('F'))
      ).repeatedItemsBetweenCompartments,
      List.empty
    )
  }

  test("Rucksack repeatedItemsBetweenCompartments with 1 repeated") {
    assertEquals(
      Rucksack(
        firstHalf = NonEmptyList.of(Item('a'), Item('b'), Item('c')),
        secondHalf = NonEmptyList.of(Item('D'), Item('a'), Item('F'))
      ).repeatedItemsBetweenCompartments,
      List(Item('a'))
    )
  }

  test("Rucksack repeatedItemsBetweenCompartments with 2 repeated") {
    assertEquals(
      Rucksack(
        firstHalf = NonEmptyList.of(Item('a'), Item('b'), Item('c')),
        secondHalf = NonEmptyList.of(Item('c'), Item('C'), Item('a'))
      ).repeatedItemsBetweenCompartments,
      List(Item('a'), Item('c'))
    )
  }

  test("getRepeatedItemsPrioritiesSum on small input") {
    val input =
      """
        |vJrwpWtwJgWrhcsFMMfFFhFp
        |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        |PmmdzqPrVvPwwTWBwg
        |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        |ttgJtRGJQctTZtZT
        |CrZsJsPPZsGzwwsLwLmpwMDw
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getRepeatedItemsPrioritiesSum(input), Some(157))
  }

  test("getRepeatedItemsPrioritiesSum(bigInput) returns valid value") {
    assertEquals(getRepeatedItemsPrioritiesSum(bigInput), Some(7_903))
  }

  test("getGroupedBadgeItemsPrioritiesSum on small input") {
    val input =
      """
        |vJrwpWtwJgWrhcsFMMfFFhFp
        |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        |PmmdzqPrVvPwwTWBwg
        |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        |ttgJtRGJQctTZtZT
        |CrZsJsPPZsGzwwsLwLmpwMDw
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getGroupedBadgeItemsPrioritiesSum(input), Some(70))
  }

  test("getGroupedBadgeItemsPrioritiesSum(bigInput) returns valid value") {
    assertEquals(getGroupedBadgeItemsPrioritiesSum(bigInput), Some(2_548))
  }

}
object Day3Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day3_input.txt")

}
