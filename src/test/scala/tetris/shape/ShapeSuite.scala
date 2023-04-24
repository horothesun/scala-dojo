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
    val obtained = r.bottomHoleBordered(2).rightFilledBordered(Green).transposed
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

  property("HStack(s.splittedByFilledColumns).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(HStack(s.splittedByFilledColumns).standardized, s.standardized)
    }
  }

  property("HStack(splittedCols).splittedByFilledColumns.standardized = splittedCols.standardized") {
    forAll(splittedByColumnsShapesGen(focus = AllFilled, Height(10), primaryColorGen)) { splittedCols =>
      assertEquals(
        HStack(splittedCols).splittedByFilledColumns.map(_.standardized),
        splittedCols.map(_.standardized)
      )
    }
  }

  property("vStack(s.splittedByFilledRows).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(vStack(s.splittedByFilledRows).standardized, s.standardized)
    }
  }

  property("vStack(splittedRows).splittedByFilledRows.standardized = splittedRows.standardized") {
    forAll(splittedByRowsShapesGen(focus = AllFilled, Width(10), primaryColorGen)) { splittedRows =>
      assertEquals(
        vStack(splittedRows).splittedByFilledRows.map(_.standardized),
        splittedRows.map(_.standardized)
      )
    }
  }

  property("HStack(s.splittedByHoleColumns).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(HStack(s.splittedByHoleColumns).standardized, s.standardized)
    }
  }

  property("HStack(splittedCols).splittedByHoleColumns.standardized = splittedCols.standardized") {
    forAll(splittedByColumnsShapesGen(focus = AllHoles, Height(10), primaryColorGen)) { splittedCols =>
      assertEquals(
        HStack(splittedCols).splittedByHoleColumns.map(_.standardized),
        splittedCols.map(_.standardized)
      )
    }
  }

  property("vStack(s.splittedByHoleRows).standardized = s.standardized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(vStack(s.splittedByHoleRows).standardized, s.standardized)
    }
  }

  property("vStack(splittedRows).splittedByHoleRows.standardized = splittedRows.standardized") {
    forAll(splittedByRowsShapesGen(focus = AllHoles, Width(10), primaryColorGen)) { splittedRows =>
      assertEquals(
        vStack(splittedRows).splittedByHoleRows.map(_.standardized),
        splittedRows.map(_.standardized)
      )
    }
  }

  property("s.standardized preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      val std = s.standardized
      assertEquals(std.rasterized, s.rasterized)
      assertEquals(std.width, s.width)
      assertEquals(std.height, s.height)
    }
  }

}
