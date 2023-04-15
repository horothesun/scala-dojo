package tetris.shape

import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary._
import Generators._
import ShapeLawsSuite._

class ShapeLawsSuite extends DisciplineSuite {

  checkAll("Shape.FunctorLaws", FunctorTests[Shape].functor[Int, Int, String])

}

object ShapeLawsSuite {

  implicit def shapeArb[A: Arbitrary]: Arbitrary[Shape[A]] = Arbitrary(shapeGen(arbitrary[A]))

}
