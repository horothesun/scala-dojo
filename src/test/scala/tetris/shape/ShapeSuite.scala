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
import ShapeSuite._

class ShapeSuite extends ScalaCheckSuite {

  property("HFlipped(HFlipped(s)) preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(HFlipped(HFlipped(s))), rasterAndSizes(s))
    }
  }

  property("VFlipped(VFlipped(s)) preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(VFlipped(VFlipped(s))), rasterAndSizes(s))
    }
  }

  property("Transposed(Transposed(s)) preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(Transposed(Transposed(s))), rasterAndSizes(s))
    }
  }

  property("(Inverted(ifHole = Red, _) ^ 2) preserves rasterization and sizes, for Red-only Shapes") {
    forAll(shapeGen(redColorGen)) { s =>
      val inverted: Endo[Shape[Red.type]] = Inverted(ifHole = Red, _)
      assertEquals(rasterAndSizes((inverted ^ 2)(s)), rasterAndSizes(s))
    }
  }

  property("HStack(s.splittedByFilledColumns) preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(HStack(s.splittedByFilledColumns)), rasterAndSizes(s))
    }
  }

  property("HStack(splittedCols).splittedByFilledColumns preserves splittedCols rasterization and sizes") {
    forAll(splittedByColumnsShapesGen(focus = AllFilled, Height(10), primaryColorGen)) { splittedCols =>
      assertEquals(
        HStack(splittedCols).splittedByFilledColumns.map(rasterAndSizes),
        splittedCols.map(rasterAndSizes)
      )
    }
  }

  property("vStack(s.splittedByFilledRows) preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(vStack(s.splittedByFilledRows)), rasterAndSizes(s))
    }
  }

  property("vStack(splittedRows).splittedByFilledRows preserves splittedRows rasterization and sizes") {
    forAll(splittedByRowsShapesGen(focus = AllFilled, Width(10), primaryColorGen)) { splittedRows =>
      assertEquals(
        vStack(splittedRows).splittedByFilledRows.map(rasterAndSizes),
        splittedRows.map(rasterAndSizes)
      )
    }
  }

  property("HStack(s.splittedByHoleColumns) preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(HStack(s.splittedByHoleColumns)), rasterAndSizes(s))
    }
  }

  property("HStack(splittedCols).splittedByHoleColumns preserves splittedCols rasterization and sizes") {
    forAll(splittedByColumnsShapesGen(focus = AllHoles, Height(10), primaryColorGen)) { splittedCols =>
      assertEquals(
        HStack(splittedCols).splittedByHoleColumns.map(rasterAndSizes),
        splittedCols.map(rasterAndSizes)
      )
    }
  }

  property("vStack(s.splittedByHoleRows) preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(vStack(s.splittedByHoleRows)), rasterAndSizes(s))
    }
  }

  property("vStack(splittedRows).splittedByHoleRows preserves splittedRows rasterization and sizes") {
    forAll(splittedByRowsShapesGen(focus = AllHoles, Width(10), primaryColorGen)) { splittedRows =>
      assertEquals(
        vStack(splittedRows).splittedByHoleRows.map(rasterAndSizes),
        splittedRows.map(rasterAndSizes)
      )
    }
  }

  property("s.standardized preserves s rasterization and sizes") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(rasterAndSizes(s.standardized), rasterAndSizes(s))
    }
  }

}

object ShapeSuite {

  def rasterAndSizes[A](s: Shape[A]): (Width, Height, Raster[A]) = (s.width, s.height, s.rasterized)

}
