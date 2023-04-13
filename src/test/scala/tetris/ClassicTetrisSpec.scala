package tetris

import munit.ScalaCheckSuite
import shape.Shape._
import ClassicTetris._
import Models._
import Models.Color._
import Models.MergedIntersection._

class ClassicTetrisSpec extends ScalaCheckSuite {

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
    assertEquals(
      mergedIntersection(
        bottomLeft1 = Coord(x = 0, y = 0),
        s1 = vStack(f, h).leftHoleBordered().leftFilledBordered(Mono),
        bottomLeft2 = Coord(x = 1, y = 0),
        s2 = j.rotatedCW
      ),
      ValidIntersection(
        bottomLeft = Coord(x = 1, y = 0),
        hStack(f, f).vRepeated(2)
      )
    )
  }

  test("mergedIntersection(Coord(0,0), f, Coord(0,0), f) = CollidingIntersection(Coord(0,0), f, f)") {
    assertEquals(
      mergedIntersection(
        bottomLeft1 = Coord(x = 0, y = 0),
        s1 = f,
        bottomLeft2 = Coord(x = 0, y = 0),
        s2 = f
      ),
      CollidingIntersection(
        bottomLeft = Coord(x = 0, y = 0),
        intersection1 = f,
        intersection2 = f
      )
    )
  }

  test("mergedIntersection on more complex shapes returns CollidingIntersection") {
    assertEquals(
      mergedIntersection(
        bottomLeft1 = Coord(x = 0, y = 0),
        s1 = vStack(f, h).leftHoleBordered().leftFilledBordered(Mono),
        bottomLeft2 = Coord(x = 2, y = 0),
        s2 = j.rotatedCW
      ),
      CollidingIntersection(
        bottomLeft = Coord(x = 2, y = 0),
        intersection1 = vStack(f, h),
        intersection2 = vStack(f, f)
      )
    )
  }

}
