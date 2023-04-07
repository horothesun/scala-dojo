package tetris

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import Models._
import Models.Color._
import Models.MergedIntersection._
import shape._
import shape.Models._
import shape.Shape._

object ClassicTetris {

  val h = Hole[Color]()
  val f = Filled[Color](Mono)
  val hf = hStack(h, f)
  val ff = f.hRepeated(2)
  val fff = f.hRepeated(3)
  val fhf = hStack(f, h, f)
  val hff = hStack(h, f, f)

  val i = f.vRepeated(4)
  val o = ff.vRepeated(2)
  val t = hStack(h, f, h).topFilledBordered(Mono)
  val j = f.vRepeated(3).leftHoleBordered.bottomFilledBordered(Mono)
  val l = j.vFlipped
  val s = vStack(hff, ff)
  val z = s.vFlipped
  val allTetrominoes = NonEmptyList.of[Shape[Color]](i, o, t, j, l, s, z)

  def mergedIntersection[A](
    bottomLeft1: Coord,
    s1: Shape[A],
    bottomLeft2: Coord,
    s2: Shape[A]
  ): MergedIntersection[A] =
    if (s1.isEmpty || s2.isEmpty) ValidIntersection(Empty[A]())
    else
      intersections(bottomLeft1, s1, bottomLeft2, s2)
        .fold[MergedIntersection[A]](ifEmpty = NotIntersecting[A]()) { case (i1, i2) =>
          i1.mergedWith(i2).fold[MergedIntersection[A]](ifEmpty = CollidingIntersection(i1, i2))(ValidIntersection[A])
        }

  // pre-condition: both Shapes are NOT empty
  def intersections[A](
    bottomLeft1: Coord,
    s1: Shape[A],
    bottomLeft2: Coord,
    s2: Shape[A]
  ): Option[(Shape[A], Shape[A])] =
    intersectionBottomLeftAndTopRightCoords(
      bottomLeft1,
      topRight1 = Coord(x = bottomLeft1.x + s1.width.value - 1, y = bottomLeft1.y + s1.height.value - 1),
      bottomLeft2,
      topRight2 = Coord(x = bottomLeft2.x + s2.width.value - 1, y = bottomLeft2.y + s2.height.value - 1)
    ).flatMap { case (intersectionBottomLeft, intersectionTopRight) =>
      val windowed = windowedShape[A](intersectionBottomLeft, intersectionTopRight) _
      (windowed(bottomLeft1, s1), windowed(bottomLeft2, s2)).tupled
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

  /*
   y
   /\
   |
   |          0 | 1 | 2 |
   |      0 |   |   |   |
   |      1 |   | ® | ® |
   |      2 |   | ® | ® |
   2      3 |   |   |   |
   |
   -----------5----------------> x

   windowTopRight   = Coord(x = 7, y = 4)
   windowBottomLeft = Coord(x = 6, y = 3)
   bottomLeft = Coord(x = 5, y = 2)
   s.width = 3
   s.height = 4

   localRight = windowTopRight.x   - bottomLeft.x = 7 - 5 = 2
   localLeft  = windowBottomLeft.x - bottomLeft.x = 6 - 5 = 1

   localTop    = (s.height - 1) - (windowTopRight.y   - bottomLeft.y) = (4 - 1) - (4 - 2) = 3 - 2 = 1
   localBottom = (s.height - 1) - (windowBottomLeft.y - bottomLeft.y) = (4 - 1) - (3 - 2) = 3 - 1 = 2

   windowTopRight(local) = col: 2, row: 1
   windowBottomLeft(local) = col: 1, row: 2
   */
  def windowedShape[A](windowBottomLeft: Coord, windowTopRight: Coord)(
    bottomLeft: Coord,
    s: Shape[A]
  ): Option[Shape[A]] =
    Some((windowBottomLeft, windowTopRight, bottomLeft)).filter { case (wbl, wtr, bl) =>
      ((wbl.x == wtr.x && wbl.y == wtr.y) || (wbl.x < wtr.x && wbl.y < wtr.y)) &&
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

  val plus = vStack(hf, fff, hf)
  val times = vStack(fhf, hf, fhf)
  val diamond = times.inverted(ifHole = Mono)
  val squareBorder = h.filledBordered(Mono)
  def squaredTarget[A](n: Int, a: A): Shape[A] = {
    val fs = List.fill[ShapeEndo[A]](n)(_.holeBordered.filledBordered(a))
    Foldable[List].fold(fs).apply(filled(a))
  }
  def spiral[A](n: Int, a: A): Shape[A] = {
    val fs = List.fill[ShapeEndo[A]](n)(
      _.bottomHoleBordered
        .rightFilledBordered(a)
        .leftHoleBordered
        .bottomFilledBordered(a)
        .topHoleBordered
        .leftFilledBordered(a)
        .rightHoleBordered
        .topFilledBordered(a)
    )
    Foldable[List].fold(fs).apply(filled(a))
  }
  val allComplexShapes = NonEmptyList.of[Shape[Color]](
    plus,
    times,
    diamond,
    squareBorder,
    squaredTarget[Color](2, Mono),
    spiral[Color](2, Mono)
  )

  def showEmptyGrid(hole: => String, width: Width, height: Height): String = {
    val leftBorder = "<!"
    val rightBorder = "!>"
    val emptyRow = List.fill(width.value)(hole).mkString(leftBorder, "", rightBorder)
    val bottomBorder = List.fill(width.value)("==").mkString(leftBorder, "", rightBorder) ++
      "\n" ++ List.fill(width.value)("\\/").mkString("  ", "", "  ")
    (List.fill(height.value)(emptyRow) :+ bottomBorder).mkString("\n")
  }

  def shapeToString(s: Shape[Color]): String = s.show(filled = { case Mono => "🟩" }, hole = "⬜️")

  def mergedIntersectionToString(mi: MergedIntersection[Color]): String =
    mi.show(filled = { case Mono => "🟩" }, hole = "⬜️")

  def main(args: Array[String]): Unit = {
//    println(showEmptyGrid(hole = " .", Width(10), Height(20)))
//    println("\n---")
    println(
      allTetrominoes
        .concatNel(allComplexShapes)
        .map(shapeToString)
        .toList
        .mkString("\n", "\n\n", "\n")
    )
    println("\n---\n")
    val complex = // empty.filledBorder(Mono)
      vStack(
        hStack(t, f.vRepeated(2)),
        i.rotatedCCW.vRepeated(3),
        s,
        i.rotatedCCW
      )
    println(shapeToString(complex))
    println("\n---\n")
    println(
      complex.splittedByFilledRows
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    val myShape01 = vStack(diamond.leftHoleBordered.holeBordered, hStack(h, h, f)).bottomHoleBordered.bottomHoleBordered
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
      vStack(hStack(h, f), hStack(f, h)).leftHoleBordered.topHoleBordered.bottomHoleBordered.hRepeated(2).vRepeated(3)
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
      List(slash, backSlash, slash.mergedWith(backSlash).get)
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    println(
      List(times, diamond, times.mergedWith(diamond).get)
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    val myShape03 = vStack(f, h).leftHoleBordered.leftFilledBordered(Mono)
    val myShape04 = j.rotatedCW
    println(
      List(myShape03, myShape04)
        .map(shapeToString)
        .appended(
          mergedIntersectionToString(
            mergedIntersection(
              bottomLeft1 = Coord(x = 0, y = 0),
              s1 = myShape03,
              bottomLeft2 = Coord(x = 1, y = 0),
              s2 = myShape04
            )
          )
        )
        .mkString("\n\n")
    )
  }

}
