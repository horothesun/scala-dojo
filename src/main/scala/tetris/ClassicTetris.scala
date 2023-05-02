package tetris

import cats.data.NonEmptyList
import cats.implicits._
import EndoOps._
import Models._
import Models.MergedIntersection._
import scala.math._
import shape._
import shape.Models._
import shape.Shape._

object ClassicTetris {

  def mergedIntersection[A](
    bottomLeft1: Coord,
    s1: Shape[A],
    bottomLeft2: Coord,
    s2: Shape[A]
  ): MergedIntersection[A] =
    if (s1.isEmpty || s2.isEmpty) ValidIntersection(bottomLeft1, Empty[A]()) // TODO: check!!! 🔥🔥🔥
    else
      intersections(bottomLeft1, s1, bottomLeft2, s2)
        .fold[MergedIntersection[A]](ifEmpty = NotIntersecting[A]()) { case (bl, i1, i2) =>
          i1.exclusivelyMergedWith(i2)
            .fold[MergedIntersection[A]](ifEmpty = CollidingIntersection(bl, i1, i2))(ValidIntersection[A](bl, _))
        }

  // pre-condition: both Shapes are NOT empty
  def intersections[A](
    bottomLeft1: Coord,
    s1: Shape[A],
    bottomLeft2: Coord,
    s2: Shape[A]
  ): Option[(Coord, Shape[A], Shape[A])] =
    intersectionBottomLeftAndTopRightCoords(
      bottomLeft1,
      topRight1 = Coord(x = bottomLeft1.x + s1.width.value - 1, y = bottomLeft1.y + s1.height.value - 1),
      bottomLeft2,
      topRight2 = Coord(x = bottomLeft2.x + s2.width.value - 1, y = bottomLeft2.y + s2.height.value - 1)
    ).flatMap { case (intersectionBottomLeft, intersectionTopRight) =>
      val windowed = windowedShape[A](intersectionBottomLeft, intersectionTopRight) _
      (Some(intersectionBottomLeft), windowed(bottomLeft1, s1), windowed(bottomLeft2, s2)).tupled
    }

  // pre-condition: both Shapes are NOT empty
  def intersectionBottomLeftAndTopRightCoords(
    bottomLeft1: Coord,
    topRight1: Coord,
    bottomLeft2: Coord,
    topRight2: Coord
  ): Option[(Coord, Coord)] =
    (
      intersectionTop(top1 = topRight1.y, bottom1 = bottomLeft1.y, top2 = topRight2.y, bottom2 = bottomLeft2.y),
      intersectionBottom(top1 = topRight1.y, bottom1 = bottomLeft1.y, top2 = topRight2.y, bottom2 = bottomLeft2.y),
      intersectionLeft(left1 = bottomLeft1.x, right1 = topRight1.x, left2 = bottomLeft2.x, right2 = topRight2.x),
      intersectionRight(left1 = bottomLeft1.x, right1 = topRight1.x, left2 = bottomLeft2.x, right2 = topRight2.x)
    ).tupled.map { case (t, b, l, r) => (Coord(x = l, y = b), Coord(x = r, y = t)) }

  // pre-condition: both Shapes are NOT empty
  def intersectionTop(top1: Int, bottom1: Int, top2: Int, bottom2: Int): Option[Int] =
    if (top1 < bottom2 || top2 < bottom1) None
    else if (top1 <= top2) Some(top1)
    else Some(top2)
  def intersectionBottom(top1: Int, bottom1: Int, top2: Int, bottom2: Int): Option[Int] =
    if (top1 < bottom2 || top2 < bottom1) None
    else if (bottom1 <= bottom2) Some(bottom2)
    else Some(bottom1)
  def intersectionLeft(left1: Int, right1: Int, left2: Int, right2: Int): Option[Int] =
    if (right1 < left2 || right2 < left1) None
    else if (left1 <= left2) Some(left2)
    else Some(left1)
  def intersectionRight(left1: Int, right1: Int, left2: Int, right2: Int): Option[Int] =
    if (right1 < left2 || right2 < left1) None
    else if (right1 <= right2) Some(right1)
    else Some(right2)

  def windowedShape[A](windowBottomLeft: Coord, windowTopRight: Coord)(
    bottomLeft: Coord,
    s: Shape[A]
  ): Option[Shape[A]] =
    Some((windowBottomLeft, windowTopRight, bottomLeft)).filter { case (wbl, wtr, bl) =>
      wbl.x <= wtr.x &&
      wbl.y <= wtr.y &&
      wbl.x >= bl.x && bl.x + s.width.value > wtr.x &&
      wbl.y >= bl.y && bl.y + s.height.value > wtr.y
    }.map { case (wbl, wtr, bl) =>
      val localTop = (s.height.value - 1) - (wtr.y - bl.y)
      val localBottom = (s.height.value - 1) - (wbl.y - bl.y)
      val localRight = wtr.x - bl.x
      val localLeft = wbl.x - bl.x
      val localHeight = localBottom - localTop + 1
      val localWidth = localRight - localLeft + 1
      val rows = s.rasterized.value
        .slice(localTop, localTop + localHeight)
        .map(r => r.slice(localLeft, localLeft + localWidth))
      fromRaster(Raster(rows))
    }

  def merge[A](bottomLeftFront: Coord, front: Shape[A], bottomLeftBack: Coord, back: Shape[A]): (Coord, Shape[A]) = {
    val topFrontToBack = (bottomLeftBack.y + back.height.value) - (bottomLeftFront.y + front.height.value)
    val leftFrontToBack = bottomLeftFront.x - bottomLeftBack.x
    val extendedFront = front.leftHoleBordered(max(0, leftFrontToBack)).topHoleBordered(max(0, topFrontToBack))
    val extendedBack = back.leftHoleBordered(max(0, -leftFrontToBack)).topHoleBordered(max(0, -topFrontToBack))
    val bottomLeft = Coord(x = min(bottomLeftFront.x, bottomLeftBack.x), y = min(bottomLeftFront.y, bottomLeftBack.y))
    (bottomLeft, extendedFront.above(extendedBack))
  }

  def i[A](a: A): Shape[A] = filled(a).vRepeated(4)
  def o[A](a: A): Shape[A] = filled(a).hRepeated(2).vRepeated(2)
  def t[A](a: A): Shape[A] = filled(a).leftHoleBordered().rightHoleBordered().topFilledBordered(a)
  def j[A](a: A): Shape[A] = filled(a).vRepeated(3).leftHoleBordered().bottomFilledBordered(a)
  def l[A](a: A): Shape[A] = j(a).vFlipped
  def s[A](a: A): Shape[A] = vStack(filled(a).hRepeated(2).leftHoleBordered(), filled(a).hRepeated(2))
  def z[A](a: A): Shape[A] = s(a).vFlipped
  val allTetrominoesMono = NonEmptyList.of[Mono.type => Shape[Mono.type]](i, o, t, j, l, s, z).map(_(Mono))

  val h = Hole[Mono.type]()
  val f = Filled(Mono)
  val hf = hStack(h, f)
  val ff = f.hRepeated(2)
  val fff = f.hRepeated(3)
  val fhf = hStack(f, h, f)
  val hff = hStack(h, f, f)

  val plus = vStack(hf, fff, hf)
  val times = vStack(fhf, hf, fhf)
  val diamond = times.inverted(ifHole = Mono)
  val squareBorder = h.filledBordered(Mono)
  def squaredTarget[A](n: Int, a: A): Shape[A] =
    n match {
      case _ if n <= 0 => empty
      case _           => repeat[Shape[A]](_.holeBordered().filledBordered(a), n - 1)(filled(a))
    }
  def spiral[A](n: Int, a: A): Shape[A] = {
    def rightOpenSpiral(n: Int): Shape[A] =
      repeat[Shape[A]](
        _.bottomHoleBordered()
          .rightFilledBordered(a)
          .leftHoleBordered()
          .bottomFilledBordered(a)
          .topHoleBordered()
          .leftFilledBordered(a)
          .rightHoleBordered()
          .topFilledBordered(a),
        n
      )(filled(a))
    n match {
      case _ if n < 0  => empty
      case _ if n == 0 => rightOpenSpiral(n)
      case _           => rightOpenSpiral(n).rightFilledBordered(a)
    }
  }

  val allComplexShapes = NonEmptyList.of[Shape[Mono.type]](
    plus,
    times,
    diamond,
    squareBorder,
    squaredTarget(4, Mono),
    spiral(3, Mono)
  )

  def showEmptyGrid(hole: => String, width: Width, height: Height): String = {
    val leftBorder = "<!"
    val rightBorder = "!>"
    val emptyRow = List.fill(width.value)(hole).mkString(leftBorder, "", rightBorder)
    val bottomBorder = List.fill(width.value)("==").mkString(leftBorder, "", rightBorder) ++
      "\n" ++ List.fill(width.value)("\\/").mkString("  ", "", "  ")
    (List.fill(height.value)(emptyRow) :+ bottomBorder).mkString("\n")
  }

  def shapeToString(s: Shape[Mono.type]): String = s.show(filled = { case Mono => "🟩" }, hole = "⬜️")

  def mergedIntersectionToString(mi: MergedIntersection[Mono.type]): String =
    mi.show(filled = { case Mono => "🟩" }, hole = "⬜️")

  def main(args: Array[String]): Unit = {
//    println(showEmptyGrid(hole = " .", Width(10), Height(20)))
//    println("\n---")
    println(
      allTetrominoesMono
        .concatNel(allComplexShapes)
        .map(shapeToString)
        .toList
        .mkString("\n", "\n\n", "\n")
    )
    println("\n---\n")
    val complex = // empty.filledBorder(Mono)
      vStack(
        hStack(t(Mono), f.vRepeated(2)),
        i(Mono).rotatedCCW.vRepeated(3),
        s(Mono),
        i(Mono).rotatedCCW
      )
    println(shapeToString(complex))
    println("\n---\n")
    println(
      complex.splitByFilledRows
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    val myShape01 = vStack(
      diamond.leftHoleBordered().holeBordered(),
      hStack(h, h, f)
    ).bottomHoleBordered().bottomHoleBordered()
    println(shapeToString(myShape01))
    val vTrimmed01 = myShape01.vHoleTrimmed
    println(s"\nvTrimmed01.top: ${vTrimmed01.top}")
    println(s"vTrimmed01.bottom: ${vTrimmed01.bottom}")
    println(s"vTrimmed01.trimmed:\n${shapeToString(vTrimmed01.trimmed)}")
    val hTrimmed01 = myShape01.hHoleTrimmed
    println(s"\nhTrimmed01.left: ${hTrimmed01.left}")
    println(s"hTrimmed01.right: ${hTrimmed01.right}")
    println(s"hTrimmed01.trimmed:\n${shapeToString(hTrimmed01.trimmed)}")
    val trimmed01 = myShape01.holeTrimmed
    println(s"trimmed01.top: ${trimmed01.top}")
    println(s"trimmed01.bottom: ${trimmed01.bottom}")
    println(s"trimmed01.left: ${trimmed01.left}")
    println(s"trimmed01.right: ${trimmed01.right}")
    println(s"trimmed01.trimmed:\n${shapeToString(trimmed01.trimmed)}")
    println("\n---\n")
    val myShape02 =
      vStack(hStack(h, f), hStack(f, h))
        .leftHoleBordered()
        .topHoleBordered()
        .bottomHoleBordered()
        .hRepeated(2)
        .vRepeated(3)
    val cutMyShape02 =
      windowedShape(windowBottomLeft = Coord(x = 0, y = 3), windowTopRight = Coord(x = 2, y = 4))(
        bottomLeft = Coord(x = -2, y = 2),
        myShape02
      ).get
    println(
      List(myShape02, cutMyShape02)
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    val slash = vStack(hStack(h, f), hStack(f, h))
    val backSlash = slash.vFlipped
    println(
      List(slash, backSlash, slash.exclusivelyMergedWith(backSlash).get)
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    println(
      List(times, diamond, times.exclusivelyMergedWith(diamond).get)
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    val myShape03 = vStack(f, h).leftHoleBordered().leftFilledBordered(Mono)
    val myShape04 = j(Mono).rotatedCW
    println(
      List(myShape03, myShape04)
        .map(shapeToString)
        .appended(
          mergedIntersectionToString(
            mergedIntersection(
              bottomLeft1 = Coord(x = 0, y = 0),
              s1 = myShape03,
              bottomLeft2 = Coord(x = 2, y = 0),
              s2 = myShape04
            )
          )
        )
        .mkString("\n\n")
    )
    println("\n---\n")
    val myShape05 = f.hRepeated(5).topHoleBordered()
    val myShape06 = vStack(l(Mono).rotatedCCW, f.hRepeated(2))
    println(
      List(myShape05, myShape06, myShape05.above(myShape06), myShape05.below(myShape06))
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    println(
      List(slash, slash.hSym, slash.vSym, slash.quarterSym)
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    val (bottomLeft07, myShape07) = merge[Mono.type](
      bottomLeftFront = Coord(x = -1, y = -1),
      front = o(Mono),
      bottomLeftBack = Coord(x = 2, y = -4),
      back = s(Mono)
    )
    println(
      List(s(Mono), o(Mono), myShape07)
        .map(shapeToString)
        .appended(bottomLeft07.toString)
        .mkString("\n\n")
    )
  }

}
