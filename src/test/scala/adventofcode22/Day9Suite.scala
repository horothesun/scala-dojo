package adventofcode22

import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import Day9._
import Day9.Rope._
import Day9Suite._

class Day9Suite extends ScalaCheckSuite {

  test("Rope.fromNel(NEL(1, 2, 3)) == Segment(Knot(1), Segment(Knot(2), Knot(3)))") {
    assertEquals(
      Rope.fromNel(NonEmptyList.of(1, 2, 3)),
      Segment(Knot(1), Segment(Knot(2), Knot(3)))
    )
  }

  test("Rope.make2(0) == Segment(Knot(0), Knot(0))") {
    assertEquals(Rope.make2(0), Segment(Knot(0), Knot(0)))
  }

  test("Segment(Knot(1), Segment(Knot(2), Knot(3))).getHead == Knot(1)") {
    val rope: Rope[Int] = Segment(Knot(1), Segment(Knot(2), Knot(3)))
    assertEquals(rope.getHead, Knot(1))
  }

  test("Segment(Knot(1), Segment(Knot(2), Knot(3))).getLast == Knot(3)") {
    val rope: Rope[Int] = Segment(Knot(1), Segment(Knot(2), Knot(3)))
    assertEquals(rope.getLast, Knot(3))
  }

}
object Day9Suite {

//  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day9_input.txt")

}
