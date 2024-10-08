package adventofcode22

import Day9._
import Day9.Proximity._
import Day9.Rope._
import Day9Suite._
import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class Day9Suite extends ScalaCheckSuite {

  test("Rope.fromNel(NEL(1, 2, 3)) == Segment(1, Segment(2, Knot(3)))") {
    assertEquals(Rope.fromNel(NonEmptyList.of(1, 2, 3)), Segment(1, Segment(2, Knot(3))))
  }

  test("Rope.make2(0) == Segment(0, Knot(0))") {
    assertEquals(Rope.make2(0), Segment(0, Knot(0)))
  }

  test("Rope.make2(0).length == 2") {
    assertEquals(Rope.make2(0).length, 2)
  }

  test("Rope.make10(0) == Segment(Knot(0), ...) 10 times") {
    assertEquals(Rope.make10(0), Rope.fromNel(NonEmptyList.of(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
  }

  test("Rope.make10(0).length == 10") {
    assertEquals(Rope.make10(0).length, 10)
  }

  test("Segment(Knot(1), Segment(2, Knot(3))).getHead == 1") {
    val rope: Rope[Int] = Segment(1, Segment(2, Knot(3)))
    assertEquals(rope.getHead, 1)
  }

  test("Segment(1, Segment(2, Knot(3))).getLast == 3") {
    val rope: Rope[Int] = Segment(1, Segment(2, Knot(3)))
    assertEquals(rope.getLast, 3)
  }

  test("Knot(1).toNel == NEL(1)") {
    assertEquals(Knot(1).toNel, NonEmptyList.one(1))
  }

  test("Segment(1, Segment(2, Knot(3))).toNel == NEL(1, 2, 3)") {
    assertEquals(Segment(1, Segment(2, Knot(3))).toNel, NonEmptyList.of(1, 2, 3))
  }

  property("Rope.fromNel(nel).toNel == nel") {
    forAll(nelGen(Gen.alphaChar)) { nel =>
      assertEquals(Rope.fromNel(nel).toNel, nel)
    }
  }

  test("Knot(1).scanLeft(10)(_ + _) == Segment(10, Knot(11))") {
    val rope: Rope[Int] = Knot(1)
    assertEquals(rope.scanLeft[Int](10)(_ + _), Segment(10, Knot(11)))
  }

  test("Segment(1, Segment(2, Knot(3))).scanLeft(10)(_ + _) == Segment(10, Segment(11, Segment(13, Knot(16))))") {
    val rope: Rope[Int] = Segment(1, Segment(2, Knot(3)))
    assertEquals(rope.scanLeft[Int](10)(_ + _), Segment(10, Segment(11, Segment(13, Knot(16)))))
  }

  property("Rope scanLeft increases length by 1") {
    forAll(ropeGen(Gen.posNum[Int])) { r =>
      assertEquals(r.scanLeft(0)(_ + _).length, 1 + r.length)
    }
  }

  property("Pos p is adjacent to p.move(d) for any p and d") {
    forAll(positionGen, directionGen) { case (p, d) =>
      assertEquals(p.getProximity(p.move(d)), Adjacent)
    }
  }

  test("Pos(1, 1).getNewFollowerPos(Pos(3, 1)) == Pos(2, 1)") {
    assertEquals(Pos(1, 1).getNewFollowerPos(Pos(3, 1)), Pos(2, 1))
  }

  test("Pos(1, 3).getNewFollowerPos(Pos(1, 1)) == Pos(1, 2)") {
    assertEquals(Pos(1, 3).getNewFollowerPos(Pos(1, 1)), Pos(1, 2))
  }

  test("Pos(1, 1).getNewFollowerPos(Pos(2, 3)) == Pos(2, 2)") {
    assertEquals(Pos(1, 1).getNewFollowerPos(Pos(2, 3)), Pos(2, 2))
  }

  test("Pos(1, 1).getNewFollowerPos(Pos(3, 2)) == Pos(2, 2)") {
    assertEquals(Pos(1, 1).getNewFollowerPos(Pos(3, 2)), Pos(2, 2))
  }

  property("p.getNewFollowerPos(p.move(d)) == p for any Pos p and Direction d") {
    forAll(positionGen, directionGen) { case (p, d) =>
      assertEquals(p.getNewFollowerPos(p.move(d)), p)
    }
  }

  test("parse motions (small input)") {
    val input =
      """
        |R 4
        |U 4
        |L 3
        |D 1
        |R 4
        |D 1
        |L 5
        |R 2
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getMotions(input), Some(motions))
  }

  test("getSingleSteps returns correct value (small input)") {
    assertEquals(
      getSingleSteps(motions),
      List(
        Direction.Right,
        Direction.Right,
        Direction.Right,
        Direction.Right,
        Direction.Up,
        Direction.Up,
        Direction.Up,
        Direction.Up,
        Direction.Left,
        Direction.Left,
        Direction.Left,
        Direction.Down,
        Direction.Right,
        Direction.Right,
        Direction.Right,
        Direction.Right,
        Direction.Down,
        Direction.Left,
        Direction.Left,
        Direction.Left,
        Direction.Left,
        Direction.Left,
        Direction.Right,
        Direction.Right
      )
    )
  }

  property("getSingleSteps preserves total number of single steps for any Motion list") {
    forAll(Gen.listOf(motionGen)) { motions =>
      assertEquals(getSingleSteps(motions).length, motions.map(_.steps).sum)
    }
  }

  test("getRopeAfterHeadMoves(Right, H(0,0)~T(0,0)) == H(1,0)~T(0,0)") {
    val rope: Rope[Pos] = Rope.make2(Pos.start)
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(1, 0), tail = Knot(Pos(0, 0)))
    )
  }

  test("getRopeAfterHeadMoves(Right, H(1,0)~T(0,0)) == H(2,0)~T(1,0)") {
    val rope: Rope[Pos] = Segment(head = Pos(1, 0), tail = Knot(Pos(0, 0)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(2, 0), tail = Knot(Pos(1, 0)))
    )
  }

  test("getRopeAfterHeadMoves(Right, H(2,0)~T(1,0)) == H(3,0)~T(2,0)") {
    val rope: Rope[Pos] = Segment(head = Pos(2, 0), tail = Knot(Pos(1, 0)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(3, 0), tail = Knot(Pos(2, 0)))
    )
  }

  test("getRopeAfterHeadMoves(Right, H(3,0)~T(2,0)) == H(4,0)~T(3,0)") {
    val rope: Rope[Pos] = Segment(head = Pos(3, 0), tail = Knot(Pos(2, 0)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(4, 0), tail = Knot(Pos(3, 0)))
    )
  }

  test("getRopeAfterHeadMoves(Up, H(4,0)~T(3,0)) == H(4,1)~T(3,0)") {
    val rope: Rope[Pos] = Segment(head = Pos(4, 0), tail = Knot(Pos(3, 0)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Up, rope),
      Segment(head = Pos(4, 1), tail = Knot(Pos(3, 0)))
    )
  }

  test("getRopeAfterHeadMoves(Up, H(4,1)~T(3,0)) == H(4,2)~T(4,1)") {
    val rope: Rope[Pos] = Segment(head = Pos(4, 1), tail = Knot(Pos(3, 0)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Up, rope),
      Segment(head = Pos(4, 2), tail = Knot(Pos(4, 1)))
    )
  }

  test("getRopeAfterHeadMoves(Up, H(4,2)~T(4,1)) == H(4,3)~T(4,2)") {
    val rope: Rope[Pos] = Segment(head = Pos(4, 2), tail = Knot(Pos(4, 1)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Up, rope),
      Segment(head = Pos(4, 3), tail = Knot(Pos(4, 2)))
    )
  }

  test("getRopeAfterHeadMoves(Up, H(4,3)~T(4,2)) == H(4,4)~T(4,3)") {
    val rope: Rope[Pos] = Segment(head = Pos(4, 3), tail = Knot(Pos(4, 2)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Up, rope),
      Segment(head = Pos(4, 4), tail = Knot(Pos(4, 3)))
    )
  }

  test("getRopeAfterHeadMoves(Left, H(4,4)~T(4,3)) == H(3,4)~T(4,3)") {
    val rope: Rope[Pos] = Segment(head = Pos(4, 4), tail = Knot(Pos(4, 3)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Left, rope),
      Segment(head = Pos(3, 4), tail = Knot(Pos(4, 3)))
    )
  }

  test("getRopeAfterHeadMoves(Left, H(3,4)~T(4,3)) == H(2,4)~T(3,4)") {
    val rope: Rope[Pos] = Segment(head = Pos(3, 4), tail = Knot(Pos(4, 3)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Left, rope),
      Segment(head = Pos(2, 4), tail = Knot(Pos(3, 4)))
    )
  }

  test("getRopeAfterHeadMoves(Left, H(2,4)~T(3,4)) == H(1,4)~T(2,4)") {
    val rope: Rope[Pos] = Segment(head = Pos(2, 4), tail = Knot(Pos(3, 4)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Left, rope),
      Segment(head = Pos(1, 4), tail = Knot(Pos(2, 4)))
    )
  }

  test("getRopeAfterHeadMoves(Down, H(1,4)~T(2,4)) == H(1,3)~T(2,4)") {
    val rope: Rope[Pos] = Segment(head = Pos(1, 4), tail = Knot(Pos(2, 4)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Down, rope),
      Segment(head = Pos(1, 3), tail = Knot(Pos(2, 4)))
    )
  }

  test("getRopeAfterHeadMoves(Right, H(1,3)~T(2,4)) == H(2,3)~T(2,4)") {
    val rope: Rope[Pos] = Segment(head = Pos(1, 3), tail = Knot(Pos(2, 4)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(2, 3), tail = Knot(Pos(2, 4)))
    )
  }

  test("getRopeAfterHeadMoves(Right, H(2,3)~T(2,4)) == H(3,3)~T(2,4)") {
    val rope: Rope[Pos] = Segment(head = Pos(2, 3), tail = Knot(Pos(2, 4)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(3, 3), tail = Knot(Pos(2, 4)))
    )
  }

  test("getRopeAfterHeadMoves(Right, H(3,3)~T(2,4)) == H(4,3)~T(3,3)") {
    val rope: Rope[Pos] = Segment(head = Pos(3, 3), tail = Knot(Pos(2, 4)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(4, 3), tail = Knot(Pos(3, 3)))
    )
  }

  test("getRopeAfterHeadMoves(Right, H(4,3)~T(3,3)) == H(5,3)~T(4,3)") {
    val rope: Rope[Pos] = Segment(head = Pos(4, 3), tail = Knot(Pos(3, 3)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Right, rope),
      Segment(head = Pos(5, 3), tail = Knot(Pos(4, 3)))
    )
  }

  test("getDistinctRopeTailPositionCount(Rope.make2(Pos.start), motions) == 13 (small input)") {
    assertEquals(getDistinctRopeTailPositionCount(initialRope = Rope.make2(Pos.start), motions), 13)
  }

  test("getDistinctRopeTailPositionCount(Rope.make2(Pos.start), bigInput) == Some(6284)") {
    assertEquals(getDistinctRopeTailPositionCount(initialRope = Rope.make2(Pos.start), bigInput), Some(6284))
  }

  test(
    "getRopeAfterHeadMoves(Up, H(4,1)~1(3,0)~2(2,0)~3(1,0)~4(0,0)~5(0,0))" +
      " == H(4,2)~1(4,1)~2(3,1)~3(2,1)~4(1,1)~5(0,0)"
  ) {
    val rope = Rope.fromNel[Pos](NonEmptyList.of(Pos(4, 1), Pos(3, 0), Pos(2, 0), Pos(1, 0), Pos(0, 0), Pos(0, 0)))
    assertEquals(
      getRopeAfterHeadMoves(Direction.Up, rope),
      Rope.fromNel[Pos](NonEmptyList.of(Pos(4, 2), Pos(4, 1), Pos(3, 1), Pos(2, 1), Pos(1, 1), Pos(0, 0)))
    )
  }

  test("getDistinctRopeTailPositionCount(Rope.make10(Pos.start), motions) == 1 (small input)") {
    assertEquals(getDistinctRopeTailPositionCount(initialRope = Rope.make10(Pos.start), motions), 1)
  }

  test("getDistinctRopeTailPositionCount(Rope.make10(Pos.start), motionsMedium) == 123") {
    assertEquals(getDistinctRopeTailPositionCount(initialRope = Rope.make10(Pos.start), motionsMedium), 36)
  }

  test("getDistinctRopeTailPositionCount(bigInput, Rope.make10(Pos.start)) == Some(2661)") {
    assertEquals(getDistinctRopeTailPositionCount(initialRope = Rope.make10(Pos.start), bigInput), Some(2661))
  }

}
object Day9Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day9_input.txt")

  val motions: List[Motion] = List(
    Motion(Direction.Right, 4),
    Motion(Direction.Up, 4),
    Motion(Direction.Left, 3),
    Motion(Direction.Down, 1),
    Motion(Direction.Right, 4),
    Motion(Direction.Down, 1),
    Motion(Direction.Left, 5),
    Motion(Direction.Right, 2)
  )

  val motionsMedium: List[Motion] = List(
    Motion(Direction.Right, 5),
    Motion(Direction.Up, 8),
    Motion(Direction.Left, 8),
    Motion(Direction.Down, 3),
    Motion(Direction.Right, 17),
    Motion(Direction.Down, 10),
    Motion(Direction.Left, 25),
    Motion(Direction.Up, 20)
  )

  def ropeGen[A](aGen: Gen[A]): Gen[Rope[A]] = Gen.lzy(Gen.oneOf(knotGen(aGen), segmentGen(aGen)))
  def knotGen[A](aGen: Gen[A]): Gen[Knot[A]] = aGen.map(Knot.apply)
  def segmentGen[A](aGen: Gen[A]): Gen[Segment[A]] =
    Gen.zip(aGen, ropeGen(aGen)).map { case (a, r) => Segment(head = a, tail = r) }

  def nelGen[A](aGen: Gen[A]): Gen[NonEmptyList[A]] = Gen.posNum[Int].flatMap(n => nelGen(n, aGen))
  def nelGen[A](n: Int, aGen: Gen[A]): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(n - 1, aGen)).map { case (a, as) => NonEmptyList(a, as) }

  def directionGen: Gen[Direction] = Gen.oneOf(Direction.values.toSeq)

  def positionGen: Gen[Pos] = {
    val intGen = Gen.chooseNum(-100, 100)
    Gen.zip(intGen, intGen).map { case (x, y) => Pos(x, y) }
  }

  def motionGen: Gen[Motion] = Gen.zip(directionGen, Gen.posNum[Int]).map((Motion.apply _).tupled)

}
