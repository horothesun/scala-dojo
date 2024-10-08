package tetris

import ClassicTetris._
import Models._
import Models.MergedIntersection._
import munit.ScalaCheckSuite
import shape.Generators.PrimaryColor
import shape.Generators.PrimaryColor._
import shape.Shape._

class ClassicTetrisSuite extends ScalaCheckSuite {

  /*
     y
     /\
     |
     |          0 | 1 | 2 |
     |      0 |   |   |   |
     |      1 |   | º | º |
     |      2 |   | º | º |
     2      3 |   |   |   |
     |
     -----------5----------------> x
   */
  test("windowedShape on valid window bounds") {
    val expected = hStack(f, h).bottomFilledBordered(Mono)
    val shape = expected.topHoleBordered().bottomHoleBordered().leftHoleBordered()
    assertEquals(
      windowedShape(windowBottomLeft = Coord(x = 6, y = 3), windowTopRight = Coord(x = 7, y = 4))(
        bottomLeft = Coord(x = 5, y = 2),
        shape
      ).map(_.standardized),
      Some(expected.standardized)
    )
  }

  /*
     y
     /\
     |
     |          0 | 1 | 2 |
     |      0 |   |   |   |
     |      1 |   | º | º | º
     |      2 |   | º | º | º
     2      3 |   |   |   |
     |
     -----------5----------------> x
   */
  test("windowedShape on invalid window bounds") {
    val shape = hStack(f, h).bottomFilledBordered(Mono).topHoleBordered().bottomHoleBordered().leftHoleBordered()
    assertEquals(
      windowedShape(windowBottomLeft = Coord(x = 6, y = 3), windowTopRight = Coord(x = 8, y = 4))(
        bottomLeft = Coord(x = 5, y = 2),
        shape
      ),
      None
    )
  }

  test("mergedIntersection returns NotIntersecting") {
    assertEquals(
      mergedIntersection(
        bottomLeft1 = Coord(x = 0, y = 0),
        s1 = f,
        bottomLeft2 = Coord(x = 1, y = 1),
        s2 = f
      ),
      NotIntersecting[Mono.type]()
    )
  }

  test("mergedIntersection returns ValidIntersection") {
    mergedIntersection(
      bottomLeft1 = Coord(x = 0, y = 0),
      s1 = vStack(f, h).leftHoleBordered().leftFilledBordered(Mono),
      bottomLeft2 = Coord(x = 1, y = 0),
      s2 = j(Mono).rotatedCW
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
      s2 = j(Mono).rotatedCW
    ) match {
      case NotIntersecting() | ValidIntersection(_, _) => assert(cond = false, "Not CollidingIntersection(...)")
      case CollidingIntersection(bottomLeft, intersection1, intersection2) =>
        assertEquals(bottomLeft, Coord(x = 2, y = 0))
        assertEquals(intersection1.standardized, vStack(f, h).standardized)
        assertEquals(intersection2.standardized, vStack(f, f).standardized)
    }
  }

  test("merge on not overlapping Shapes") {
    val (bottomLeft, merged) = merge[Mono.type](
      bottomLeftFront = Coord(x = -1, y = -1),
      front = o(Mono),
      bottomLeftBack = Coord(x = 2, y = -4),
      back = s(Mono)
    )
    assertEquals(bottomLeft, Coord(x = -1, y = -4))
    assertEquals(
      merged.standardized,
      vStack(o(Mono), h, s(Mono).leftHoleBordered(3)).standardized
    )
  }

  test("merge on overlapping Shapes w/o collision") {
    val (bottomLeft, merged) = merge[Mono.type](
      bottomLeftFront = Coord(x = -1, y = -1),
      front = o(Mono),
      bottomLeftBack = Coord(x = 0, y = -2),
      back = s(Mono)
    )
    val ff_hf = hStack(h, f).topFilledBordered(Mono)
    assertEquals(bottomLeft, Coord(x = -1, y = -2))
    assertEquals(
      merged.standardized,
      hStack(ff_hf.topFilledBordered(Mono), ff_hf.vFlipped.topHoleBordered()).standardized
    )
  }

  test("merge on overlapping Shapes with collision") {
    val redO = filled[PrimaryColor](Red).hRepeated(2).vRepeated(2)
    val g = filled[PrimaryColor](Green)
    val h = hole[PrimaryColor]
    val greenS = vStack(hStack(h, g, g), hStack(g, g))
    val (bottomLeft, merged) = merge[PrimaryColor](
      bottomLeftFront = Coord(x = -1, y = -1),
      front = redO,
      bottomLeftBack = Coord(x = 0, y = -1),
      back = greenS
    )
    assertEquals(bottomLeft, Coord(x = -1, y = -1))
    assertEquals(
      merged.standardized,
      hStack(redO, vStack(g, h).leftFilledBordered(Green)).standardized
    )
  }

}
