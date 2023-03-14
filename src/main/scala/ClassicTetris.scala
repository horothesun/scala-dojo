import cats.{Foldable, MonoidK}
import cats.data.NonEmptyList
import scala.annotation.tailrec
import scala.collection.immutable.Nil
import ClassicTetris.Color._
import ClassicTetris.Shape._

object ClassicTetris {

  case class Width(value: Int) {
    def `+`(that: Width): Width = Width(this.value + that.value)
  }
  object Width {
    def max(x: Width, y: Width): Width = if (x.value >= y.value) x else y
  }

  case class Height(value: Int) {
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
    val rasterize: List[List[Option[A]]]

    def transposed: Shape[A] = Shape.transposed(this)

    def rotatedCW: Shape[A] = Shape.rotatedCW(this)
    def rotatedCCW: Shape[A] = Shape.rotatedCCW(this)

    def show(filled: A => String, hole: => String): String =
      this.rasterize.map(_.map(_.fold(ifEmpty = hole)(filled)).mkString("")).mkString("\n")
  }
  object Shape {

    def empty[A]: Shape[A] = MonoidK[Shape].empty
    def hole[A]: Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override lazy val rasterize: List[List[Option[A]]] = List(List(None))
    }
    def filled[A](a: A): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override lazy val rasterize: List[List[Option[A]]] = List(List(Some(a)))
    }

    def rotatedCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterize: List[List[Option[A]]] = transpose(s.rasterize).map(_.reverse)
    }
    def rotatedCCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterize: List[List[Option[A]]] = transpose(s.rasterize).reverse
    }

    def hStack[A](l: Shape[A], rs: Shape[A]*): Shape[A] = hStack(l :: rs.toList)
    def hStack[A](ss: List[Shape[A]]): Shape[A] = Foldable[List].foldK(ss)
    def hStack[A](l: Shape[A], r: Shape[A]): Shape[A] = MonoidK[Shape].combineK(l, r)

    def vStack[A](t: Shape[A], bs: Shape[A]*): Shape[A] = vStack(t :: bs.toList)
    def vStack[A](ss: List[Shape[A]]): Shape[A] = ss.fold(MonoidK[Shape].empty)(vStack(_: Shape[A], _: Shape[A]))
    def vStack[A](t: Shape[A], b: Shape[A]): Shape[A] = hStack(b.rotatedCW, t.rotatedCW).rotatedCCW

    def transposed[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override lazy val rasterize: List[List[Option[A]]] = transpose(s.rasterize)
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

    implicit val horizontalMonoidK: MonoidK[Shape] = new MonoidK[Shape] {
      override def empty[A]: Shape[A] = new Shape[A] {
        override lazy val width: Width = Width(0)
        override lazy val height: Height = Height(0)
        override lazy val rasterize: List[List[Option[A]]] = List.empty
      }
      override def combineK[A](x: Shape[A], y: Shape[A]): Shape[A] = new Shape[A] {
        override lazy val width: Width = x.width + y.width
        override lazy val height: Height = Height.max(x.height, y.height)
        override lazy val rasterize: List[List[Option[A]]] = {
          val renderedX =
            if (x.height >= y.height) x.rasterize
            else {
              val numbOfMissingRows = (y.height - x.height).value
              val missingRow = List.fill[Option[A]](x.width.value)(None)
              x.rasterize ++ List.fill(numbOfMissingRows)(missingRow)
            }
          val renderedY =
            if (y.height >= x.height) y.rasterize
            else {
              val numbOfMissingRows = (x.height - y.height).value
              val missingRow = List.fill[Option[A]](y.width.value)(None)
              y.rasterize ++ List.fill(numbOfMissingRows)(missingRow)
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
  val fh = hStack(f, h)
  val ff = hStack(f, f)
  val fff = hStack(f, f, f)
  val fhf = hStack(f, h, f)
  val hfh = hStack(h, f, h)
  val hff = hStack(h, f, f)
  val ffh = hStack(List(f, f, h))

  val i = vStack(f, f, f, f)
  val o = vStack(ff, ff)
  val t = vStack(fff, hfh)
  val j = vStack(hf, hf, hf, ff)
  val l = vStack(List(fh, fh, fh, ff))
  val s = vStack(hff, ffh)
  val z = vStack(ffh, hff)
  val allTetrominoes = NonEmptyList.of(i, o, t, j, l, s, z)

  val plus = vStack(hfh, fff, hfh)
  val times = vStack(fhf, hfh, fhf)
  val diamond = vStack(hfh, fhf, hfh)
  val squareBorder = vStack(fff, fhf, fff)

  def showEmptyGrid(hole: => String, width: Width, height: Height): String = {
    val leftBorder = "<!"
    val rightBorder = "!>"
    val emptyRow = List.fill(width.value)(hole).mkString(leftBorder, "", rightBorder)
    val bottomBorder = List.fill(width.value)("==").mkString(leftBorder, "", rightBorder) ++
      "\n" ++ List.fill(width.value)("\\/").mkString("  ", "", "  ")
    (List.fill(height.value)(emptyRow) :+ bottomBorder).mkString("\n")
  }

  def main(args: Array[String]): Unit =
//    println(showEmptyGrid(hole = " .", Width(10), Height(20)))
//    println("\n---")
    println(
      allTetrominoes
        .concatNel(allTetrominoes.map(_.transposed))
        .concat(List(plus, times, diamond, squareBorder))
        .toList
        .map(_.show(filled = _ => "🟩", hole = "⬜️"))
        .mkString("\n", "\n\n---\n\n", "\n")
    )

}
