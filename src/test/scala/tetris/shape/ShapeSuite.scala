package tetris.shape

import cats.Endo
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import tetris.EndoOps._
import Generators._
import Generators.FilledState._
import Generators.PrimaryColor._
import Models._
import Shape._

class ShapeSuite extends ScalaCheckSuite {

  property("s.standardized preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      val std = s.standardized
      assertEquals(std.rasterized, s.rasterized)
      assertEquals(std.width, s.width)
      assertEquals(std.height, s.height)
    }
  }

  property("HFlipped(HFlipped(s)).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(HFlipped(HFlipped(s)).standardized, s.standardized)
    }
  }

  property("VFlipped(VFlipped(s)).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(VFlipped(VFlipped(s)).standardized, s.standardized)
    }
  }

  test("Transposed sample") {
    val r = filled[PrimaryColor](Red)
    val obtained = Transposed(r.bottomHoleBordered(2).rightFilledBordered(Green))
    val expected = r.rightHoleBordered(2).bottomFilledBordered(Green)
    assertEquals(obtained.standardized, expected.standardized)
  }

  property("Transposed(Transposed(s)).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(Transposed(Transposed(s)).standardized, s.standardized)
    }
  }

  property("(Inverted(ifHole = Red, _) ^ 2) preserves standardization, for Red-only Shapes") {
    forAll(shapeGen(redColorGen)) { s =>
      val inverted: Endo[Shape[Red.type]] = Inverted(ifHole = Red, _)
      assertEquals((inverted ^ 2)(s).standardized, s.standardized)
    }
  }

  property("HStack(s.splitByFilledColumns).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(HStack(s.splitByFilledColumns).standardized, s.standardized)
    }
  }

  property("HStack(splitCols).splitByFilledColumns.standardized = splitCols.standardized") {
    forAll(splitByColumnsShapesGen(focus = AllFilled, Height(10), primaryColorGen)) { splitCols =>
      assertEquals(
        HStack(splitCols).splitByFilledColumns.map(_.standardized),
        splitCols.map(_.standardized)
      )
    }
  }

  property("vStack(s.splitByFilledRows).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(vStack(s.splitByFilledRows).standardized, s.standardized)
    }
  }

  property("vStack(splitRows).splitByFilledRows.standardized = splitRows.standardized") {
    forAll(splitByRowsShapesGen(focus = AllFilled, Width(10), primaryColorGen)) { splitRows =>
      assertEquals(
        vStack(splitRows).splitByFilledRows.map(_.standardized),
        splitRows.map(_.standardized)
      )
    }
  }

  property("HStack(s.splitByHoleColumns).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(HStack(s.splitByHoleColumns).standardized, s.standardized)
    }
  }

  property("HStack(splitCols).splitByHoleColumns.standardized = splitCols.standardized") {
    forAll(splitByColumnsShapesGen(focus = AllHoles, Height(10), primaryColorGen)) { splitCols =>
      assertEquals(
        HStack(splitCols).splitByHoleColumns.map(_.standardized),
        splitCols.map(_.standardized)
      )
    }
  }

  property("vStack(s.splitByHoleRows).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(vStack(s.splitByHoleRows).standardized, s.standardized)
    }
  }

  property("vStack(splitRows).splitByHoleRows.standardized = splitRows.standardized") {
    forAll(splitByRowsShapesGen(focus = AllHoles, Width(10), primaryColorGen)) { splitRows =>
      assertEquals(
        vStack(splitRows).splitByHoleRows.map(_.standardized),
        splitRows.map(_.standardized)
      )
    }
  }

  property("s => s.above(Inverted(c, s)).validatedAllFilled = Some(_)") {
    forAll(shapeGen(primaryColorGen), primaryColorGen) { case (s, c) =>
      val sAboveInv = s.above(Inverted(ifHole = c, s))
      assertEquals(sAboveInv.validatedAllFilled, Some(sAboveInv))
    }
  }

  property("s => s.below(Inverted(c, s)).validatedAllFilled = Some(_)") {
    forAll(shapeGen(primaryColorGen), primaryColorGen) { case (s, c) =>
      val sBelowInv = s.below(Inverted(ifHole = c, s))
      assertEquals(sBelowInv.validatedAllFilled, Some(sBelowInv))
    }
  }

  property("s.above(Inverted(c, s)) = Inverted(c, s).above(s)") {
    forAll(shapeGen(primaryColorGen), primaryColorGen) { case (s, c) =>
      val inv = Inverted(ifHole = c, s)
      assertEquals(s.above(inv), inv.above(s))
    }
  }

  property("s.below(Inverted(c, s)) = Inverted(c, s).below(s)") {
    forAll(shapeGen(primaryColorGen), primaryColorGen) { case (s, c) =>
      val inv = Inverted(ifHole = c, s)
      assertEquals(s.below(inv), inv.below(s))
    }
  }

  property("s.above(Inverted(c, s)) = s.below(Inverted(c, s))") {
    forAll(shapeGen(primaryColorGen), primaryColorGen) { case (s, c) =>
      val inv = Inverted(ifHole = c, s)
      assertEquals(s.above(inv), s.below(inv))
    }
  }

  property("Inverted(c, s).above(s) = Inverted(c, s).below(s)") {
    forAll(shapeGen(primaryColorGen), primaryColorGen) { case (s, c) =>
      val inv = Inverted(ifHole = c, s)
      assertEquals(inv.above(s), inv.below(s))
    }
  }

}
