package tetris.shape

import cats.Endo
import org.scalacheck.Gen
import Generators.PrimaryColor._
import Generators.FilledState._
import Models._
import Shape._

object Generators {

  def rasterGen[A](aGen: Gen[A]): Gen[Raster[A]] = shapeGen(aGen).map(_.rasterized)

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

  // ==========================================================================

  sealed trait FilledState
  object FilledState {
    case object AllHoles extends FilledState
    case object AllFilled extends FilledState
    case object Interleaved extends FilledState
  }
  val filledStateGen: Gen[FilledState] = Gen.oneOf(AllHoles, AllFilled, Interleaved)
  def otherFilledStateGen(focus: FilledState): Gen[FilledState] =
    focus match {
      case AllHoles    => Gen.oneOf(AllFilled, Interleaved)
      case AllFilled   => Gen.oneOf(AllHoles, Interleaved)
      case Interleaved => Gen.oneOf(AllHoles, AllFilled)
    }
  def interleavedFilledStateAndReps(n: Int, focus: FilledState, maxReps: Int): Gen[List[(FilledState, Int)]] = {
    val halfN = n / 2
    val focusesGen = Gen.const(List.fill(n - halfN)(focus))
    val othersGen = Gen.listOfN(halfN, otherFilledStateGen(focus))
    val repsGen = Gen.listOfN(n, Gen.chooseNum(1, maxReps))
    Gen
      .zip(focusesGen, othersGen, repsGen)
      .map { case (fgs, ogs, rgs) => interleave(fgs, ogs).toList.zip(rgs) }
  }

  def hAllHolesShapeGen[A](width: Width): Gen[Shape[A]] = Gen.const(hole[A].hRepeated(width.value))
  def vAllHolesShapeGen[A](height: Height): Gen[Shape[A]] = Gen.const(hole[A].vRepeated(height.value))

  def hAllFilledShapeGen[A](width: Width, aGen: Gen[A]): Gen[Shape[A]] =
    Gen.listOfN(width.value, filledGen(aGen)).map(HStack.apply)
  def vAllFilledShapeGen[A](height: Height, aGen: Gen[A]): Gen[Shape[A]] =
    Gen.listOfN(height.value, filledGen(aGen)).map(vStack(_: List[Shape[A]]))

  def hInterleavedShapeGen[A](width: Width, aGen: Gen[A]): Gen[Shape[A]] = {
    val halfWidth = width.value / 2
    val hs = List.fill(width.value - halfWidth)(hole[A])
    Gen.listOfN(halfWidth, filledGen(aGen)).map(fs => hStack(interleave(fs, hs).toList))
  }
  def vInterleavedShapeGen[A](height: Height, aGen: Gen[A]): Gen[Shape[A]] = {
    val halfHeight = height.value / 2
    val hs = List.fill(height.value - halfHeight)(hole[A])
    Gen.listOfN(halfHeight, filledGen(aGen)).map(fs => vStack(interleave(fs, hs).toList))
  }

  def splittedByRowsShapesGen[A](focus: FilledState, width: Width, aGen: Gen[A]): Gen[List[Shape[A]]] =
    splittedShapesGen(
      allHolesShapeGen = r => hAllHolesShapeGen[A](Width(r)),
      allFilledShapeGen = r => hAllFilledShapeGen[A](Width(r), aGen),
      interleavedShapeGen = r => hInterleavedShapeGen(Width(r), aGen),
      repeat = r => _.vRepeated(r),
      focus,
      fixedDimensionSize = width.value
    )
  def splittedByColumnsShapesGen[A](focus: FilledState, height: Height, aGen: Gen[A]): Gen[List[Shape[A]]] =
    splittedShapesGen(
      allHolesShapeGen = r => vAllHolesShapeGen[A](Height(r)),
      allFilledShapeGen = r => vAllFilledShapeGen[A](Height(r), aGen),
      interleavedShapeGen = r => vInterleavedShapeGen(Height(r), aGen),
      repeat = r => _.hRepeated(r),
      focus,
      fixedDimensionSize = height.value
    )
  def splittedShapesGen[A](
    allHolesShapeGen: Int => Gen[Shape[A]],
    allFilledShapeGen: Int => Gen[Shape[A]],
    interleavedShapeGen: Int => Gen[Shape[A]],
    repeat: Int => Endo[Shape[A]],
    focus: FilledState,
    fixedDimensionSize: Int
  ): Gen[List[Shape[A]]] =
    Gen
      .chooseNum(2, 5)
      .flatMap(n => interleavedFilledStateAndReps(n, focus, maxReps = 3))
      .flatMap(frs =>
        Gen.sequence[List[Shape[A]], Shape[A]](
          frs.map { case (f, r) =>
            (f match {
              case AllHoles    => allHolesShapeGen
              case AllFilled   => allFilledShapeGen
              case Interleaved => interleavedShapeGen
            })(fixedDimensionSize).map(repeat(r))
          }
        )
      )

  def interleave[A](a: Seq[A], b: Seq[A]): Seq[A] =
    if (a.isEmpty) b else if (b.isEmpty) a else a.head +: b.head +: interleave(a.tail, b.tail)

}
