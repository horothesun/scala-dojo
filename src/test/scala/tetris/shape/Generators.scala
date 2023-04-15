package tetris.shape

import org.scalacheck.Gen
import Generators.PrimaryColor._
import Shape._

object Generators {

  val evenGen: Gen[Int] = Gen.oneOf(Gen.const(0), Gen.posNum[Int].map(_ * 2))

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
    Gen.choose(1, 5).flatMap(n => Gen.listOfN(n, shapeGen(aGen)).map(HStack[A]))
  def invertedGen[A](aGen: Gen[A]): Gen[Shape[A]] =
    Gen.zip(aGen, shapeGen(aGen)).map { case (a, s) => Inverted[A](ifHole = a, s) }

  def rasterGen[A](aGen: Gen[A]): Gen[Raster[A]] = shapeGen(aGen).map(_.rasterized)

}
