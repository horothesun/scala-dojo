package tetris.shape

import cats.Endo
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import tetris.EndoOps._
import Generators._
import Generators.PrimaryColor._
import Shape._

class ShapeSuite extends ScalaCheckSuite {

  property("(HFlipped(_) ^ n) preserves rasterization, for n even") {
    forAll(evenGen, shapeGen(primaryColorGen)) { case (n, s) =>
      val hFlipped: Endo[Shape[PrimaryColor]] = HFlipped[PrimaryColor]
      assertEquals((hFlipped ^ n)(s).rasterized, s.rasterized)
    }
  }

  property("(VFlipped(_) ^ n) preserves rasterization, for n even") {
    forAll(evenGen, shapeGen(primaryColorGen)) { case (n, s) =>
      val vFlipped: Endo[Shape[PrimaryColor]] = VFlipped[PrimaryColor]
      assertEquals((vFlipped ^ n)(s).rasterized, s.rasterized)
    }
  }

  property("(Inverted(ifHole = Red, _) ^ n) preserves rasterization, for n even and Red-only Shapes") {
    forAll(evenGen, shapeGen(redColorGen)) { case (n, s) =>
      val inverted: Endo[Shape[Red.type]] = Inverted(ifHole = Red, _)
      assertEquals((inverted ^ n)(s).rasterized, s.rasterized)
    }
  }

  // TODO: move to a generator for Shapes with guaranteed filled columns!!! 🔥🔥🔥
  property("HStack(s.splittedByFilledColumns).rasterized = s.rasterized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(HStack(s.splittedByFilledColumns).rasterized, s.rasterized)
    }
  }

  // TODO: move to a generator for Shapes with guaranteed filled rows!!! 🔥🔥🔥
  property("vStack(s.splittedByFilledRows).rasterized = s.rasterized") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(vStack(s.splittedByFilledRows).rasterized, s.rasterized)
    }
  }

}
