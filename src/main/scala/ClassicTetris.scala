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

  trait Shape[A] {
    val width: Width
    val height: Height
    val rasterized: List[Row[A]]

    def rotatedCW: Shape[A] = Shape.rotatedCW(this)
    def rotatedCCW: Shape[A] = Shape.rotatedCCW(this)

    def hRepeated(n: Int): Shape[A] = Shape.hRepeated(n, this)
    def vRepeated(n: Int): Shape[A] = Shape.vRepeated(n, this)

    def leftFilledBorder(a: A): Shape[A] = Shape.leftFilledBorder(a, this)
    def rightFilledBorder(a: A): Shape[A] = Shape.rightFilledBorder(a, this)
    def topFilledBorder(a: A): Shape[A] = Shape.topFilledBorder(a, this)
    def bottomFilledBorder(a: A): Shape[A] = Shape.bottomFilledBorder(a, this)
    def filledBorder(a: A): Shape[A] = Shape.filledBorder(a, this)

    def leftHoleBorder: Shape[A] = Shape.leftHoleBorder(this)
    def rightHoleBorder: Shape[A] = Shape.rightHoleBorder(this)
    def topHoleBorder: Shape[A] = Shape.topHoleBorder(this)
    def bottomHoleBorder: Shape[A] = Shape.bottomHoleBorder(this)
    def holeBorder: Shape[A] = Shape.holeBorder(this)

    def inverted(ifHole: A): Shape[A] = Shape.inverted(ifHole, this)

    def splittedByFilledRows: List[Shape[A]] = Shape.splittedByFilledRows(this)
    def splittedByFilledColumns: List[Shape[A]] = Shape.splittedByFilledColumns(this)

    def splittedByHoleRows: List[Shape[A]] = Shape.splittedByHoleRows(this)
    def splittedByHoleColumns: List[Shape[A]] = Shape.splittedByHoleColumns(this)

    // TODO: implement!!! 🔥🔥🔥
    def trimEmptyBorders: Shape[A] = ???

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
    def fromRasterUnsafe[A](rows: List[Row[A]]): Shape[A] = fromRaster(rows).get

    def rotatedCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterized: List[Row[A]] = transpose(s.rasterized).map(_.reverse)
    }
    def rotatedCCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterized: List[Row[A]] = transpose(s.rasterized).reverse
    }

    def hStack[A](l: Shape[A], rs: Shape[A]*): Shape[A] = hStack(l :: rs.toList)
    def hStack[A](ss: List[Shape[A]]): Shape[A] = Foldable[List].foldK(ss)
    def hStack[A](l: Shape[A], r: Shape[A]): Shape[A] = MonoidK[Shape].combineK(l, r)

    def vStack[A](t: Shape[A], bs: Shape[A]*): Shape[A] = vStack(t :: bs.toList)
    def vStack[A](ss: List[Shape[A]]): Shape[A] = ss.fold(MonoidK[Shape].empty)(vStack(_: Shape[A], _: Shape[A]))
    def vStack[A](t: Shape[A], b: Shape[A]): Shape[A] = hStack(b.rotatedCW, t.rotatedCW).rotatedCCW

    def hRepeated[A](n: Int, s: Shape[A]): Shape[A] = hStack(List.fill(n)(s))
    def vRepeated[A](n: Int, s: Shape[A]): Shape[A] = vStack(List.fill(n)(s))

    def leftFilledBorder[A](a: A, s: Shape[A]): Shape[A] = hStack(filled(a).vRepeated(s.height.value), s)
    def rightFilledBorder[A](a: A, s: Shape[A]): Shape[A] = hStack(s, filled(a).vRepeated(s.height.value))
    def topFilledBorder[A](a: A, s: Shape[A]): Shape[A] = vStack(filled(a).hRepeated(s.width.value), s)
    def bottomFilledBorder[A](a: A, s: Shape[A]): Shape[A] = vStack(s, filled(a).hRepeated(s.width.value))
    def filledBorder[A](a: A, s: Shape[A]): Shape[A] =
      if (s.width == Width(0) || s.height == Height(0)) hStack(filled(a).leftFilledBorder(a).topFilledBorder(a))
      else s.leftFilledBorder(a).rightFilledBorder(a).topFilledBorder(a).bottomFilledBorder(a)

    def leftHoleBorder[A](s: Shape[A]): Shape[A] = hStack(hole[A], s)
    def rightHoleBorder[A](s: Shape[A]): Shape[A] = hStack(s, hole[A])
    def topHoleBorder[A](s: Shape[A]): Shape[A] = vStack(hole[A], s)
    def bottomHoleBorder[A](s: Shape[A]): Shape[A] = vStack(s, hole[A])
    def holeBorder[A](s: Shape[A]): Shape[A] =
      if (s.width == Width(0) || s.height == Height(0)) hStack(hole[A].leftHoleBorder.topHoleBorder)
      else s.leftHoleBorder.rightHoleBorder.topHoleBorder.bottomHoleBorder

    def inverted[A](ifHole: A, s: Shape[A]): Shape[A] =
      fromRasterUnsafe(s.rasterized.map(r => r.map(_.fold[Option[A]](ifEmpty = Some(ifHole))(_ => None))))

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

    def splittedByFilledRows[A](s: Shape[A]): List[Shape[A]] = splittedByValidRows(validatedFullRow, s)
    // TODO: implement!!! 🔥🔥🔥
    def splittedByFilledColumns[A](s: Shape[A]): List[Shape[A]] = ???

    def splittedByHoleRows[A](s: Shape[A]): List[Shape[A]] = splittedByValidRows(validatedEmptyRow, s)
    // TODO: implement!!! 🔥🔥🔥
    def splittedByHoleColumns[A](s: Shape[A]): List[Shape[A]] = ???

    def validatedFullRow[A](r: Row[A]): Option[Row[A]] = (r: List[Option[A]]).sequence.as(r)
    def validatedEmptyRow[A](r: Row[A]): Option[Row[A]] = Some(r).filter(_.forall(_.isEmpty)).as(r)

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
      override def pure[A](x: A): Shape[A] = filled(x)

      // TODO: implement!!! 🔥🔥🔥
      override def ap[A, B](ff: Shape[A => B])(fa: Shape[A]): Shape[B] = ???
    }

  }

  sealed trait Color
  object Color {
    case object Mono extends Color
  }

  val h: Shape[Color] = hole
  val f: Shape[Color] = filled(Mono)
  val hf = hStack(h, f)
  val ff = f.hRepeated(2)
  val fff = f.hRepeated(3)
  val fhf = hStack(f, h, f)
  val hff = hStack(h, f, f)

  val i = f.vRepeated(4)
  val o = empty[Color].filledBorder(Mono)
  val t = hStack(h, f, h).topFilledBorder(Mono)
  val j = f.vRepeated(3).leftHoleBorder.bottomFilledBorder(Mono)
  val l = f.vRepeated(3).rightHoleBorder.bottomFilledBorder(Mono)
  val s = vStack(hff, ff)
  val z = vStack(ff, hff)
  val allTetrominoes: NonEmptyList[Shape[Color]] = NonEmptyList.of(i, o, t, j, l, s, z)

  val plus = vStack(hf, fff, hf)
  val times = vStack(fhf, hf, fhf)
  val diamond = times.inverted(ifHole = Mono)
  val squareBorder = h.filledBorder(Mono)

  def showEmptyGrid(hole: => String, width: Width, height: Height): String = {
    val leftBorder = "<!"
    val rightBorder = "!>"
    val emptyRow = List.fill(width.value)(hole).mkString(leftBorder, "", rightBorder)
    val bottomBorder = List.fill(width.value)("==").mkString(leftBorder, "", rightBorder) ++
      "\n" ++ List.fill(width.value)("\\/").mkString("  ", "", "  ")
    (List.fill(height.value)(emptyRow) :+ bottomBorder).mkString("\n")
  }

  def shapeToString[A](s: Shape[A]): String = s.show(filled = _ => "🟩", hole = "⬜️")

  def main(args: Array[String]): Unit = {
//    println(showEmptyGrid(hole = " .", Width(10), Height(20)))
//    println("\n---")
    println(
      allTetrominoes
        .concat(List(plus, times, diamond, squareBorder))
        .toList
        .map(shapeToString)
        .mkString("\n", "\n\n", "\n")
    )
    println("\n---\n")
//    println(s"validatedFullRow(List.empty) = ${validatedFullRow(List.empty)}")
    val complex = // empty.filledBorder(Mono)
      vStack(
        hStack(f.vRepeated(2), t),
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
  }

}
