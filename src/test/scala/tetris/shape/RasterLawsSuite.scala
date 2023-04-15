package tetris.shape

import cats.laws.discipline.{FunctorTests, MonoidKTests}
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary._
import Generators._
import RasterLawsSuite._

class RasterLawsSuite extends DisciplineSuite {

  checkAll("Raster.FunctorLaws", FunctorTests[Raster].functor[Int, Int, String])

  checkAll("Raster.MonoidKLaws", MonoidKTests[Raster].monoidK[Int])

}

object RasterLawsSuite {

  implicit def rasterArb[A: Arbitrary]: Arbitrary[Raster[A]] = Arbitrary(rasterGen(arbitrary[A]))

}
