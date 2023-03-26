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

    def validatedAllFilled: Option[Shape[A]] = Shape.validatedAllFilledShape(this)
    def validatedAllHole: Option[Shape[A]] = Shape.validatedAllHoleShape(this)

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
      def lWidthAndLTrimmed(colSplit: List[Shape[A]]): (Width, List[Shape[A]]) =
        colSplit match {
          case Nil      => (Width(0), Nil)
          case ls :: ss => ls.validatedAllHole.fold(ifEmpty = (Width(0), colSplit))(_ => (ls.width, ss))
        }
      def rTrimmedAndRWidth(colSplit: List[Shape[A]]): (List[Shape[A]], Width) = {
        val (right, rTrimmedReversed) = lWidthAndLTrimmed(colSplit.reverse)
        (rTrimmedReversed.reverse, right)
      }
      val (left, lTrimmed) = lWidthAndLTrimmed(splittedByHoleColumns(s))
      val (trimmed, right) = rTrimmedAndRWidth(lTrimmed)
      HTrimmed(left, hStack(trimmed), right)
    }
    def vHoleTrimmed[A](s: Shape[A]): VTrimmed[A] = {
      val hTrimmed = hHoleTrimmed(s.rotatedCCW)
      VTrimmed(
        top = Height(hTrimmed.left.value),
        trimmed = hTrimmed.trimmed.rotatedCW,
        bottom = Height(hTrimmed.right.value)
      )
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

    implicit val applicative: Applicative[Shape] = new Applicative[Shape] {
      override def pure[A](a: A): Shape[A] = filled(a)

      // TODO: implement!!! 🔥🔥🔥
      override def ap[A, B](ff: Shape[A => B])(fa: Shape[A]): Shape[B] = ???
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
//    val myShape01 =
//      diamond.leftHoleBordered.holeBordered.bottomFilledBordered(Mono).bottomHoleBordered.bottomHoleBordered
//    println(shapeToString(myShape01))
//    val vTrimmed01 = myShape01.vHoleTrimmed
//    println(s"vTrimmed01.top: ${vTrimmed01.top}")
//    println(s"vTrimmed01.bottom: ${vTrimmed01.bottom}")
//    println(s"vTrimmed01.trimmed:\n${shapeToString(vTrimmed01.trimmed)}")
  }

}
