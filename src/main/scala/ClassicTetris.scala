import cats.{Foldable, MonoidK}
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
    val rasterized: List[List[Option[A]]]

    def rotatedCW: Shape[A] = Shape.rotatedCW(this)
    def rotatedCCW: Shape[A] = Shape.rotatedCCW(this)

    def hRepeated(n: Int): Shape[A] = Shape.hRepeated(n, this)
    def vRepeated(n: Int): Shape[A] = Shape.vRepeated(n, this)

    def splittedByFullRows: List[Shape[A]] = Shape.splittedByFullRows(this)

    def transposed: Shape[A] = Shape.transposed(this)

    def show(filled: A => String, hole: => String): String =
      this.rasterized.map(_.map(_.fold(ifEmpty = hole)(filled)).mkString("")).mkString("\n")
  }
  object Shape {

    def empty[A]: Shape[A] = MonoidK[Shape].empty
    def hole[A]: Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override lazy val rasterized: List[List[Option[A]]] = List(List(None))
    }
    def filled[A](a: A): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override lazy val rasterized: List[List[Option[A]]] = List(List(Some(a)))
    }
    def fromRaster[A](rows: List[List[Option[A]]]): Shape[A] = new Shape[A] {
      override val width: Width = rows match {
        case Nil    => Width(0)
        case r :: _ => Width(r.length)
      }
      override val height: Height = Height(rows.length)
      override val rasterized: List[List[Option[A]]] = rows
    }

    def rotatedCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterized: List[List[Option[A]]] = transpose(s.rasterized).map(_.reverse)
    }
    def rotatedCCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterized: List[List[Option[A]]] = transpose(s.rasterized).reverse
    }

    def hStack[A](l: Shape[A], rs: Shape[A]*): Shape[A] = hStack(l :: rs.toList)
    def hStack[A](ss: List[Shape[A]]): Shape[A] = Foldable[List].foldK(ss)
    def hStack[A](l: Shape[A], r: Shape[A]): Shape[A] = MonoidK[Shape].combineK(l, r)

    def vStack[A](t: Shape[A], bs: Shape[A]*): Shape[A] = vStack(t :: bs.toList)
    def vStack[A](ss: List[Shape[A]]): Shape[A] = ss.fold(MonoidK[Shape].empty)(vStack(_: Shape[A], _: Shape[A]))
    def vStack[A](t: Shape[A], b: Shape[A]): Shape[A] = hStack(b.rotatedCW, t.rotatedCW).rotatedCCW

    def hRepeated[A](n: Int, s: Shape[A]): Shape[A] = hStack(List.fill(n)(s))
    def vRepeated[A](n: Int, s: Shape[A]): Shape[A] = vStack(List.fill(n)(s))

    def transposed[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterized: List[List[Option[A]]] = transpose(s.rasterized)
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

    def splittedByFullRows[A](s: Shape[A]): List[Shape[A]] =
      s.rasterized
        // TODO: optimise for `::`!!! 🔥🔥🔥
        .foldLeft(List.empty[(Boolean, List[List[Option[A]]])]) { case (acc, r) =>
          val rIsFull = isFullRow(r)
          val newROnlyAcc = List((rIsFull, List(r)))
          acc.toNel.fold(ifEmpty = newROnlyAcc) { accNel =>
            val (lastIsFull, lastRows) = accNel.last
            if (rIsFull == lastIsFull) acc.dropRight(1) :+ (lastIsFull, lastRows :+ r)
            else acc ++ newROnlyAcc
          }
        }
        .map { case (_, r) => r }
        .map(fromRaster)

    def isFullRow[A](r: List[Option[A]]): Boolean = validateFullRow(r).isDefined
    def validateFullRow[A](r: List[Option[A]]): Option[List[Option[A]]] = r.sequence.as(r)

    implicit val horizontalMonoidK: MonoidK[Shape] = new MonoidK[Shape] {
      override def empty[A]: Shape[A] = new Shape[A] {
        override lazy val width: Width = Width(0)
        override lazy val height: Height = Height(0)
        override lazy val rasterized: List[List[Option[A]]] = List.empty
      }
      override def combineK[A](x: Shape[A], y: Shape[A]): Shape[A] = new Shape[A] {
        override lazy val width: Width = x.width + y.width
        override lazy val height: Height = Height.max(x.height, y.height)
        override lazy val rasterized: List[List[Option[A]]] = {
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
  val o = ff.vRepeated(2)
  val t = vStack(fff, hf)
  val j = vStack(hf.vRepeated(3), ff)
  val l = vStack(f.vRepeated(3), ff)
  val s = vStack(hff, ff)
  val z = vStack(ff, hff)
  val allTetrominoes = NonEmptyList.of(i, o, t, j, l, s, z)

  val plus = vStack(hf, fff, hf)
  val times = vStack(fhf, hf, fhf)
  val diamond = vStack(hf, fhf, hf)
  val squareBorder = vStack(fff, fhf, fff)

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
//    println(
//      allTetrominoes
//        .concatNel(allTetrominoes.map(_.transposed))
//        .concat(List(plus, times, diamond, squareBorder))
//        .toList
//        .map(shapeToString)
//        .mkString("\n", "\n\n", "\n")
//    )
//    println(s"validateFullRow(List.empty) = ${validateFullRow(List.empty)}")
    val complex = vStack(
      hStack(f.vRepeated(2), t),
      i.rotatedCCW.vRepeated(3),
      s,
      i.rotatedCCW
    )
    println(shapeToString(complex))
    println("\n---\n")
    println(
      complex.splittedByFullRows
        .map(shapeToString)
        .mkString("\n\n")
    )
  }

}
