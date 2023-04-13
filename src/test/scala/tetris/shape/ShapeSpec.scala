package tetris.shape

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import Shape._
import ShapeSpec._
import ShapeSpec.PrimaryColor._

class ShapeSpec extends ScalaCheckSuite {

  property("HFlipped(... 2*n ...HFlipped(s)) = s | n > 0") {
    forAll(Gen.posNum[Int], shapeGen(primaryColorGen)) { case (n, s) =>
      val hFlipped2n = repeat[PrimaryColor](2 * n, HFlipped.apply)
      assertEquals(hFlipped2n(s), s)
    }
  }

  property("VFlipped(... 2*n ...VFlipped(s)) = s | n > 0") {
    forAll(Gen.posNum[Int], shapeGen(primaryColorGen)) { case (n, s) =>
      val vFlipped2n = repeat[PrimaryColor](2 * n, VFlipped.apply)
      assertEquals(vFlipped2n(s), s)
    }
  }

  property("HStack(s.splittedByFilledColumns) = s") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(HStack(s.splittedByFilledColumns): Shape[PrimaryColor], s)
    }
  }

  property("vStack(s.splittedByFilledRows) = s") {
    forAll(shapeGen(primaryColorGen)) { s =>
      assertEquals(vStack(s.splittedByFilledRows), s)
    }
  }

  property("Inverted(ifHole = Red, ... 2*n ...Inverted(ifHole = Red, s)) = s | n > 0") {
    forAll(Gen.posNum[Int], shapeGen(redColorGen)) { case (n, s) =>
      val inverted2n = repeat[Red.type](2 * n, Inverted(ifHole = Red, _))
      assertEquals(inverted2n(s), s)
    }
  }

}

object ShapeSpec {

  sealed trait PrimaryColor
  object PrimaryColor {
    case object Red extends PrimaryColor
    case object Green extends PrimaryColor
    case object Blue extends PrimaryColor
  }

  val redColorGen: Gen[Red.type] = Gen.const(Red)
  val primaryColorGen: Gen[PrimaryColor] = Gen.oneOf(Red, Green, Blue)

  def shapeGen[A](aGen: Gen[A]): Gen[Shape[A]] =
    Gen.lzy(
      Gen.oneOf(
        emptyGen[A],
        holeGen[A],
        filledGen(aGen),
        hFlippedGen(aGen),
        vFlippedGen(aGen),
        transposedGen(aGen),
        hStackGen(aGen),
        invertedGen(aGen)
      )
    )
  def emptyGen[A]: Gen[Shape[A]] = Gen.const(Empty[A]())
  def holeGen[A]: Gen[Shape[A]] = Gen.const(Hole[A]())
  def filledGen[A](aGen: Gen[A]): Gen[Shape[A]] = aGen.map(a => Filled[A](a))
  def hFlippedGen[A](aGen: Gen[A]): Gen[Shape[A]] = shapeGen(aGen).map(HFlipped[A])
  def vFlippedGen[A](aGen: Gen[A]): Gen[Shape[A]] = shapeGen(aGen).map(VFlipped[A])
  def transposedGen[A](aGen: Gen[A]): Gen[Shape[A]] = shapeGen(aGen).map(Transposed[A])
  def hStackGen[A](aGen: Gen[A]): Gen[Shape[A]] =
    Gen.choose(1, 4).flatMap(n => Gen.listOfN(n, shapeGen(aGen)).map(HStack[A]))
  def invertedGen[A](aGen: Gen[A]): Gen[Shape[A]] =
    Gen.zip(aGen, shapeGen(aGen)).map { case (a, s) => Inverted[A](ifHole = a, s) }

}
