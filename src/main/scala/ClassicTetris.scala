import cats._
import cats.data.NonEmptyList
import cats.implicits._
import scala.annotation.tailrec
import scala.collection.immutable.Nil
import ClassicTetris.Color._
import ClassicTetris.Shape._

object ClassicTetris {

  case class Width(value: Int) extends AnyVal {
    def `+`(that: Width): Width = Width(this.value + that.value)
  }
  object Width {
    def max(x: Width, y: Width): Width = if (x.value >= y.value) x else y
  }

  case class Height(value: Int) extends AnyVal {
    def `+`(that: Height): Height = Height(this.value + that.value)
    def `-`(that: Height): Height = Height(this.value - that.value)
    def `>=`(that: Height): Boolean = this.value >= that.value
  }
  object Height {
    def max(x: Height, y: Height): Height = if (x.value >= y.value) x else y
  }

  case class HTrimmed[A](left: Width, trimmed: Shape[A], right: Width)
  case class VTrimmed[A](top: Height, trimmed: Shape[A], bottom: Height)
  case class Trimmed[A](top: Height, bottom: Height, left: Width, right: Width, trimmed: Shape[A])

  trait Shape[A] {
    val width: Width
    val height: Height
    val rasterized: List[Row[A]]

    def hFlipped: Shape[A] = Shape.hFlipped(this)
    def vFlipped: Shape[A] = Shape.vFlipped(this)

    def transposed: Shape[A] = Shape.transposed(this)

    def rotatedCW: Shape[A] = Shape.rotatedCW(this)
    def rotatedCCW: Shape[A] = Shape.rotatedCCW(this)

    def hRepeated(n: Int): Shape[A] = Shape.hRepeated(n, this)
    def vRepeated(n: Int): Shape[A] = Shape.vRepeated(n, this)

    def leftFilledBordered(a: A): Shape[A] = Shape.leftFilledBordered(a, this)
    def rightFilledBordered(a: A): Shape[A] = Shape.rightFilledBordered(a, this)
    def topFilledBordered(a: A): Shape[A] = Shape.topFilledBordered(a, this)
    def bottomFilledBordered(a: A): Shape[A] = Shape.bottomFilledBordered(a, this)
    def filledBordered(a: A): Shape[A] = Shape.filledBordered(a, this)

    def leftHoleBordered: Shape[A] = Shape.leftHoleBordered(this)
    def rightHoleBordered: Shape[A] = Shape.rightHoleBordered(this)
    def topHoleBordered: Shape[A] = Shape.topHoleBordered(this)
    def bottomHoleBordered: Shape[A] = Shape.bottomHoleBordered(this)
    def holeBordered: Shape[A] = Shape.holeBordered(this)

    def inverted(ifHole: A): Shape[A] = Shape.inverted(ifHole, this)

    def splittedByFilledRows: List[Shape[A]] = Shape.splittedByFilledRows(this)
    def splittedByFilledColumns: List[Shape[A]] = Shape.splittedByFilledColumns(this)

    def splittedByHoleRows: List[Shape[A]] = Shape.splittedByHoleRows(this)
    def splittedByHoleColumns: List[Shape[A]] = Shape.splittedByHoleColumns(this)

    def hHoleTrimmed: HTrimmed[A] = Shape.hHoleTrimmed(this)
    def vHoleTrimmed: VTrimmed[A] = Shape.vHoleTrimmed(this)
    def holeTrimmed: Trimmed[A] = Shape.holeTrimmed(this)

    def validatedAllFilled: Option[Shape[A]] = Shape.validatedAllFilledShape(this)
    def validatedAllHole: Option[Shape[A]] = Shape.validatedAllHoleShape(this)

    def isEmpty: Boolean = width.value < 1 || height.value < 1
    def nonEmpty: Boolean = !isEmpty

    override def equals(obj: Any): Boolean =
      obj match {
        case that: Shape[A] => width == that.width && height == that.height && rasterized == that.rasterized
        case _              => false
      }

    def map[B](f: A => B): Shape[B] = Functor[Shape].map(this)(f)

    def show(filled: A => String, hole: => String): String =
      this.rasterized.map(_.map(_.fold(ifEmpty = hole)(filled)).mkString("")).mkString("\n")
  }
  object Shape {

    type Row[A] = List[Option[A]]

    def empty[A]: Shape[A] = MonoidK[Shape].empty
    def hole[A]: Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override lazy val rasterized: List[Row[A]] = List(List(None))
    }
    def filled[A](a: A): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override lazy val rasterized: List[Row[A]] = List(List(Some(a)))
    }

    def fromRasterUnsafe[A](rows: List[Row[A]]): Shape[A] = fromRaster(rows).get
    def fromRaster[A](rows: List[Row[A]]): Option[Shape[A]] =
      rows match {
        case r :: _ if rows.exists(_.length != r.length) => None
        case _ =>
          Some(new Shape[A] {
            override val width: Width = rows match {
              case Nil    => Width(0)
              case r :: _ => Width(r.length)
            }
            override val height: Height = Height(rows.length)
            override val rasterized: List[Row[A]] = rows
          })
      }

    def hFlipped[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override val width: Width = s.width
      override val height: Height = s.height
      override val rasterized: List[Row[A]] = s.rasterized.reverse
    }
    def vFlipped[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override val width: Width = s.width
      override val height: Height = s.height
      override val rasterized: List[Row[A]] = s.rasterized.map(_.reverse)
    }

    def transposed[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override val width: Width = Width(s.height.value)
      override val height: Height = Height(s.width.value)
      override val rasterized: List[Row[A]] = transpose(s.rasterized)
    }

    def transpose[A](rows: List[List[A]]): List[List[A]] = {
      def heads(rows: List[List[A]]): List[A] = rows.map(_.head)
      def tails(rows: List[List[A]]): List[List[A]] = rows.map(_.tail)
      @tailrec
      def aux(acc: List[List[A]], rows: List[List[A]]): List[List[A]] =
        rows match {
          case Nil           => acc
          case Nil :: _      => acc
          case (_ :: _) :: _ => aux(acc :+ heads(rows), tails(rows))
        }
      aux(acc = List.empty, rows)
    }

    def rotatedCW[A](s: Shape[A]): Shape[A] = s.transposed.vFlipped
    def rotatedCCW[A](s: Shape[A]): Shape[A] = s.transposed.hFlipped

    def hStack[A](l: Shape[A], rs: Shape[A]*): Shape[A] = hStack(l :: rs.toList)
    def hStack[A](ss: List[Shape[A]]): Shape[A] = Foldable[List].foldK(ss)
    def hStack[A](l: Shape[A], r: Shape[A]): Shape[A] = MonoidK[Shape].combineK(l, r)

    def vStack[A](t: Shape[A], bs: Shape[A]*): Shape[A] = vStack(t :: bs.toList)
    def vStack[A](ss: List[Shape[A]]): Shape[A] = ss.fold(MonoidK[Shape].empty)(vStack(_: Shape[A], _: Shape[A]))
    def vStack[A](t: Shape[A], b: Shape[A]): Shape[A] = hStack(b.rotatedCW, t.rotatedCW).rotatedCCW

    def hRepeated[A](n: Int, s: Shape[A]): Shape[A] = hStack(List.fill(n)(s))
    def vRepeated[A](n: Int, s: Shape[A]): Shape[A] = vStack(List.fill(n)(s))

    def leftFilledBordered[A](a: A, s: Shape[A]): Shape[A] = hStack(filled(a).vRepeated(s.height.value), s)
    def rightFilledBordered[A](a: A, s: Shape[A]): Shape[A] = hStack(s, filled(a).vRepeated(s.height.value))
    def topFilledBordered[A](a: A, s: Shape[A]): Shape[A] = vStack(filled(a).hRepeated(s.width.value), s)
    def bottomFilledBordered[A](a: A, s: Shape[A]): Shape[A] = vStack(s, filled(a).hRepeated(s.width.value))
    def filledBordered[A](a: A, s: Shape[A]): Shape[A] =
      if (s.width == Width(0) || s.height == Height(0)) filled(a).hRepeated(2).vRepeated(2)
      else s.leftFilledBordered(a).rightFilledBordered(a).topFilledBordered(a).bottomFilledBordered(a)

    def leftHoleBordered[A](s: Shape[A]): Shape[A] = hStack(hole[A], s)
    def rightHoleBordered[A](s: Shape[A]): Shape[A] = hStack(s, hole[A])
    def topHoleBordered[A](s: Shape[A]): Shape[A] = vStack(hole[A], s)
    def bottomHoleBordered[A](s: Shape[A]): Shape[A] = vStack(s, hole[A])
    def holeBordered[A](s: Shape[A]): Shape[A] =
      if (s.width == Width(0) || s.height == Height(0)) hole[A].hRepeated(2).vRepeated(2)
      else s.leftHoleBordered.rightHoleBordered.topHoleBordered.bottomHoleBordered

    def inverted[A](ifHole: A, s: Shape[A]): Shape[A] =
      fromRasterUnsafe(s.rasterized.map(r => r.map(_.fold[Option[A]](ifEmpty = Some(ifHole))(_ => None))))

    def splittedByFilledRows[A](s: Shape[A]): List[Shape[A]] = splittedByValidRows(validatedAllFilledRow, s)
    def splittedByFilledColumns[A](s: Shape[A]): List[Shape[A]] = splittedByFilledRows(s.rotatedCW).map(_.rotatedCCW)

    def splittedByHoleRows[A](s: Shape[A]): List[Shape[A]] = splittedByValidRows(validatedAllHoleRow, s)
    def splittedByHoleColumns[A](s: Shape[A]): List[Shape[A]] = splittedByHoleRows(s.rotatedCW).map(_.rotatedCCW)

    def hHoleTrimmed[A](s: Shape[A]): HTrimmed[A] = {
      val vTrimmed = vHoleTrimmed(s.rotatedCW)
      HTrimmed(
        left = Width(vTrimmed.top.value),
        trimmed = vTrimmed.trimmed.rotatedCCW,
        right = Width(vTrimmed.bottom.value)
      )
    }
    def vHoleTrimmed[A](s: Shape[A]): VTrimmed[A] = {
      def topHeightAndTrimmed(rowSplit: List[Shape[A]]): (Height, List[Shape[A]]) =
        rowSplit match {
          case Nil      => (Height(0), Nil)
          case ts :: ss => ts.validatedAllHole.fold(ifEmpty = (Height(0), rowSplit))(_ => (ts.height, ss))
        }
      def bottomTrimmedAndHeight(rowSplit: List[Shape[A]]): (List[Shape[A]], Height) = {
        val (bottom, bTrimmedReversed) = topHeightAndTrimmed(rowSplit.reverse)
        (bTrimmedReversed.reverse, bottom)
      }
      val (top, tTrimmed) = topHeightAndTrimmed(splittedByHoleRows(s))
      val (trimmed, bottom) = bottomTrimmedAndHeight(tTrimmed)
      VTrimmed(top, vStack(trimmed), bottom)
    }
    def holeTrimmed[A](s: Shape[A]): Trimmed[A] = {
      val ht = s.hHoleTrimmed
      val vt = ht.trimmed.vHoleTrimmed
      Trimmed(top = vt.top, bottom = vt.bottom, left = ht.left, right = ht.right, trimmed = vt.trimmed)
    }

    def validatedAllFilledShape[A](s: Shape[A]): Option[Shape[A]] = s.rasterized.traverse(validatedAllFilledRow).as(s)
    def validatedAllHoleShape[A](s: Shape[A]): Option[Shape[A]] = s.rasterized.traverse(validatedAllHoleRow).as(s)

    def validatedAllFilledRow[A](r: Row[A]): Option[Row[A]] = (r: List[Option[A]]).sequence.as(r)
    def validatedAllHoleRow[A](r: Row[A]): Option[Row[A]] = Some(r).filter(_.forall(_.isEmpty)).as(r)

    def splittedByValidRows[A](validatedRow: Row[A] => Option[Row[A]], s: Shape[A]): List[Shape[A]] =
      s.rasterized
        // TODO: optimise for `::`!!! 🔥🔥🔥
        .foldLeft(List.empty[(Boolean, List[Row[A]])]) { case (acc, r) =>
          val rIsValid = validatedRow(r).isDefined
          val newROnlyAcc = List((rIsValid, List(r)))
          acc.toNel.fold(ifEmpty = newROnlyAcc) { accNel =>
            val (lastIsValid, lastRows) = accNel.last
            if (rIsValid == lastIsValid) acc.dropRight(1) :+ (lastIsValid, lastRows :+ r)
            else acc ++ newROnlyAcc
          }
        }
        .map { case (_, r) => r }
        .map(fromRasterUnsafe)

    implicit val horizontalMonoidK: MonoidK[Shape] = new MonoidK[Shape] {
      override def empty[A]: Shape[A] = new Shape[A] {
        override lazy val width: Width = Width(0)
        override lazy val height: Height = Height(0)
        override lazy val rasterized: List[Row[A]] = List.empty
      }
      override def combineK[A](x: Shape[A], y: Shape[A]): Shape[A] = new Shape[A] {
        override lazy val width: Width = x.width + y.width
        override lazy val height: Height = Height.max(x.height, y.height)
        override lazy val rasterized: List[Row[A]] = {
          val renderedX =
            if (x.height >= y.height) x.rasterized
            else {
              val numbOfMissingRows = (y.height - x.height).value
              val missingRow = List.fill[Option[A]](x.width.value)(None)
              x.rasterized ++ List.fill(numbOfMissingRows)(missingRow)
            }
          val renderedY =
            if (y.height >= x.height) y.rasterized
            else {
              val numbOfMissingRows = (x.height - y.height).value
              val missingRow = List.fill[Option[A]](y.width.value)(None)
              y.rasterized ++ List.fill(numbOfMissingRows)(missingRow)
            }
          renderedX.zip(renderedY).map { case (xRow, yRow) => xRow ++ yRow }
        }
      }
    }

    implicit val functor: Functor[Shape] = new Functor[Shape] {
      override def map[A, B](fa: Shape[A])(f: A => B): Shape[B] =
        fromRasterUnsafe(fa.rasterized.map(r => r.map(optA => optA.map(f))))
    }

  }

  sealed trait Color
  object Color {
    case object Mono extends Color
  }

  val h = hole[Color]
  val f = filled[Color](Mono)
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

  type ShapeEndo[A] = Shape[A] => Shape[A]
  implicit def shapeEndoMonoid[A]: Monoid[ShapeEndo[A]] = new Monoid[ShapeEndo[A]] {
    override def empty: ShapeEndo[A] = identity
    override def combine(f: ShapeEndo[A], g: ShapeEndo[A]): ShapeEndo[A] = f.compose(g)
  }

  case class Coord(x: Int, y: Int)

  def merged[A](bottomLeft1: Coord, s1: Shape[A], bottomLeft2: Coord, s2: Shape[A]): Option[Shape[A]] =
    if (s1.isEmpty || s2.isEmpty) Some(empty[A])
    else intersections(bottomLeft1, s1, bottomLeft2, s2).flatMap((mergedShapes[A] _).tupled)

  def atLeastOneNone[A](o1: Option[A], o2: Option[A]): Option[Option[A]] =
    (o1, o2) match {
      case (None, None)       => Some(None)
      case (Some(a1), None)   => Some(Some(a1))
      case (None, Some(a2))   => Some(Some(a2))
      case (Some(_), Some(_)) => None
    }

  def mergedShapes[A](s1: Shape[A], s2: Shape[A]): Option[Shape[A]] =
    Some((s1, s2)).filter { case (s1, s2) => s1.width == s2.width && s1.height == s2.height }.mapFilter {
      case (s1, s2) =>
        s1.rasterized.zip(s2.rasterized).traverse { case (r1, r2) => r1.zip(r2).traverse((atLeastOneNone[A] _).tupled) }
    }.map(fromRasterUnsafe)

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
    ).map { case (intersectionBottomLeft, intersectionTopRight) =>
      val intShape = intersectionShape[A](intersectionBottomLeft, intersectionTopRight) _
      (intShape(bottomLeft1, s1), intShape(bottomLeft2, s2))
    }

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

   intersectionTopRight   = Coord(x = 7, y = 4)
   intersectionBottomLeft = Coord(x = 6, y = 3)
   bottomLeft = Coord(x = 5, y = 2)
   s.width = 3
   s.height = 4

   intersectionRight(local) = intersectionTopRight.x   - bottomLeft.x = 7 - 5 = 2
   intersectionLeft(local)  = intersectionBottomLeft.x - bottomLeft.x = 6 - 5 = 1

   intersectionTop(local)    = (s.height - 1) - (intersectionTopRight.y   - bottomLeft.y) = (4 - 1) - (4 - 2) = 3 - 2 = 1
   intersectionBottom(local) = (s.height - 1) - (intersectionBottomLeft.y - bottomLeft.y) = (4 - 1) - (3 - 2) = 3 - 1 = 2

   intersectionTopRight(local) = col: 2, row: 1
   intersectionBottomLeft(local) = col: 1, row: 2
   */
  def intersectionShape[A](intersectionBottomLeft: Coord, intersectionTopRight: Coord)(
    bottomLeft: Coord,
    s: Shape[A]
  ): Shape[A] = {
    val localTop = (s.height.value - 1) - (intersectionTopRight.y - bottomLeft.y)
    val localBottom = (s.height.value - 1) - (intersectionBottomLeft.y - bottomLeft.y)
    val localRight = intersectionTopRight.x - bottomLeft.x
    val localLeft = intersectionBottomLeft.x - bottomLeft.x
    val localHeight = localBottom - localTop + 1
    val localWidth = localRight - localLeft + 1
    new Shape[A] {
      override val width: Width = Width(localWidth)
      override val height: Height = Height(localHeight)
      override val rasterized: List[Row[A]] =
        s.rasterized
          .slice(localTop, localTop + localHeight)
          .map(r => r.slice(localLeft, localLeft + localWidth))
    }
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
//    println("\n---\n")
//    val myShape01 = vStack(diamond.leftHoleBordered.holeBordered, hStack(h, h, f)).bottomHoleBordered.bottomHoleBordered
//    println(shapeToString(myShape01))
//    val vTrimmed01 = myShape01.vHoleTrimmed
//    println(s"\nvTrimmed01.top: ${vTrimmed01.top}")
//    println(s"vTrimmed01.bottom: ${vTrimmed01.bottom}")
//    println(s"vTrimmed01.trimmed:\n${shapeToString(vTrimmed01.trimmed)}")
//    val hTrimmed01 = myShape01.hHoleTrimmed
//    println(s"\nhTrimmed01.left: ${hTrimmed01.left}")
//    println(s"hTrimmed01.right: ${hTrimmed01.right}")
//    println(s"hTrimmed01.trimmed:\n${shapeToString(hTrimmed01.trimmed)}")
//    val trimmed01 = myShape01.holeTrimmed
//    println(s"trimmed01.top: ${trimmed01.top}")
//    println(s"trimmed01.bottom: ${trimmed01.bottom}")
//    println(s"trimmed01.left: ${trimmed01.left}")
//    println(s"trimmed01.right: ${trimmed01.right}")
//    println(s"trimmed01.trimmed:\n${shapeToString(trimmed01.trimmed)}")
    println("\n---\n")
    val myShape02 =
      vStack(hStack(h, f), hStack(f, h)).leftHoleBordered.topHoleBordered.bottomHoleBordered.hRepeated(2).vRepeated(3)
    println(shapeToString(myShape02))
    val cutMyShape02 =
      intersectionShape(intersectionBottomLeft = Coord(x = 6, y = 3), intersectionTopRight = Coord(x = 7, y = 4))(
        bottomLeft = Coord(x = 5, y = 2),
        myShape02
      )
    println("")
    println(shapeToString(cutMyShape02))
    println("\n---\n")
    val slash = vStack(hStack(h, f), hStack(f, h))
    val backSlash = slash.vFlipped
    println(
      List(slash, backSlash, mergedShapes(slash, backSlash).get)
        .map(shapeToString)
        .mkString("\n\n")
    )
    println("\n---\n")
    println(
      List(times, diamond, mergedShapes(times, diamond).get)
        .map(shapeToString)
        .mkString("\n\n")
    )
  }

}
