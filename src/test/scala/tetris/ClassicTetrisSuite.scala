package tetris

import munit.ScalaCheckSuite
import shape.Shape._
import ClassicTetris._
import Models._
import Models.Color._
import Models.MergedIntersection._

class ClassicTetrisSuite extends ScalaCheckSuite {

  test("mergedIntersection returns NotIntersecting") {
    assertEquals(
      mergedIntersection(
        bottomLeft1 = Coord(x = 0, y = 0),
        s1 = f,
        bottomLeft2 = Coord(x = 1, y = 1),
        s2 = f
      ),
      NotIntersecting[Color]()
    )
  }

  test("mergedIntersection returns ValidIntersection") {
    mergedIntersection(
      bottomLeft1 = Coord(x = 0, y = 0),
      s1 = vStack(f, h).leftHoleBordered().leftFilledBordered(Mono),
      bottomLeft2 = Coord(x = 1, y = 0),
      s2 = j.rotatedCW
    ) match {
      case NotIntersecting() | CollidingIntersection(_, _, _) => assert(cond = false, "Not ValidIntersection(...)")
      case ValidIntersection(bottomLeft, mergedIntersection) =>
        assertEquals(bottomLeft, Coord(x = 1, y = 0))
        assertEquals(mergedIntersection.standardized, hStack(f, f).vRepeated(2).standardized)
    }

  }

  test("mergedIntersection(Coord(0,0), f, Coord(0,0), f) = CollidingIntersection(Coord(0,0), f, f)") {
    mergedIntersection(
      bottomLeft1 = Coord(x = 0, y = 0),
      s1 = f,
      bottomLeft2 = Coord(x = 0, y = 0),
      s2 = f
    ) match {
      case NotIntersecting() | ValidIntersection(_, _) => assert(cond = false, "Not CollidingIntersection(...)")
      case CollidingIntersection(bottomLeft, intersection1, intersection2) =>
        assertEquals(bottomLeft, Coord(x = 0, y = 0))
        assertEquals(intersection1.standardized, f.standardized)
        assertEquals(intersection2.standardized, f.standardized)
    }
  }

  test("mergedIntersection on more complex shapes returns CollidingIntersection") {
    mergedIntersection(
      bottomLeft1 = Coord(x = 0, y = 0),
      s1 = vStack(f, h).leftHoleBordered().leftFilledBordered(Mono),
      bottomLeft2 = Coord(x = 2, y = 0),
      s2 = j.rotatedCW
    ) match {
      case NotIntersecting() | ValidIntersection(_, _) => assert(cond = false, "Not CollidingIntersection(...)")
      case CollidingIntersection(bottomLeft, intersection1, intersection2) =>
        assertEquals(bottomLeft, Coord(x = 2, y = 0))
        assertEquals(intersection1.standardized, vStack(f, h).standardized)
        assertEquals(intersection2.standardized, vStack(f, f).standardized)
    }
  }

}
