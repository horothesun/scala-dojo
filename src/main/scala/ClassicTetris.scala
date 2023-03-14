import cats.MonoidK
import cats.data.NonEmptyList
import scala.annotation.tailrec
import scala.collection.immutable.Nil
import ClassicTetris.Color.Mono
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
    def render: List[List[Option[A]]]

    def show(filled: A => String, hole: => String): String =
      this.render.map(_.map(_.fold(ifEmpty = hole)(filled)).mkString("")).mkString("\n")
  }
  object Shape {

    def empty[A]: Shape[A] = MonoidK[Shape].empty
    def hole[A]: Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override def render: List[List[Option[A]]] = List(List(None))
    }
    def filled[A](a: A): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
      override def render: List[List[Option[A]]] = List(List(Some(a)))
    }

    def rotatedCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
      override def render: List[List[Option[A]]] = transpose(s.render).map(_.reverse).reverse
    }

    def beside[A](l: Shape[A], r: Shape[A]): Shape[A] = MonoidK[Shape].combineK(l, r)

    def beside[A](l: Shape[A], rs: Shape[A]*): Shape[A] = if (rs.isEmpty) l else rs.fold(l)(beside)

    def topBottom[A](t: Shape[A], b: Shape[A]): Shape[A] =
      rotatedCW(rotatedCW(rotatedCW(beside(rotatedCW(b), rotatedCW(t)))))

    def topBottom[A](t: Shape[A], bs: Shape[A]*): Shape[A] = if (bs.isEmpty) t else bs.fold(t)(topBottom)

    def transpose[A](rows: List[List[A]]): List[List[A]] = {
      def heads(rows: List[List[A]]): List[A] = rows.map(_.head)
      def tails(rows: List[List[A]]): List[List[A]] = rows.map(_.tail)
      @tailrec
      def aux(acc: List[List[A]], rows: List[List[A]]): List[List[A]] =
        rows match {
          case Nil => acc
          case firstRow :: _ =>
            firstRow match {
              case Nil    => acc
              case _ :: _ => aux(heads(rows) +: acc, tails(rows))
            }
        }
      aux(acc = List.empty, rows)
    }

    implicit val horizontalMonoidK: MonoidK[Shape] = new MonoidK[Shape] {
      override def empty[A]: Shape[A] = new Shape[A] {
        override lazy val width: Width = Width(0)
        override lazy val height: Height = Height(0)
        override def render: List[List[Option[A]]] = List.empty
      }
      override def combineK[A](x: Shape[A], y: Shape[A]): Shape[A] = new Shape[A] {
        override lazy val width: Width = x.width + y.width
        override lazy val height: Height = Height.max(x.height, y.height)
        override def render: List[List[Option[A]]] = {
          val renderedX =
            if (x.height >= y.height) x.render
            else {
              val numbOfMissingRows = (y.height - x.height).value
              val missingRow = List.fill[Option[A]](x.width.value)(None)
              x.render ++ List.fill(numbOfMissingRows)(missingRow)
            }
          val renderedY =
            if (y.height >= x.height) y.render
            else {
              val numbOfMissingRows = (x.height - y.height).value
              val missingRow = List.fill[Option[A]](y.width.value)(None)
              y.render ++ List.fill(numbOfMissingRows)(missingRow)
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
  val hf = beside(h, f)
  val fh = beside(f, h)
  val ff = beside(f, f)
  val fff = beside(f, f, f)
  val fhf = beside(f, h, f)
  val hfh = beside(h, f, h)
  val hff = beside(h, f, f)
  val ffh = beside(f, f, h)

  val i = beside(f, f, f, f)
  val o = topBottom(ff, ff)
  val t = topBottom(fff, hfh)
  val j = topBottom(hf, hf, hf, ff)
  val l = topBottom(fh, fh, fh, ff)
  val s = topBottom(hff, ffh)
  val z = topBottom(ffh, hff)
  val allTetrominoes = NonEmptyList.of(i, o, t, j, l, s, z)

  val plus = topBottom(hfh, fff, hfh)
  val times = topBottom(fhf, hfh, fhf)
  val diamond = topBottom(hfh, fhf, hfh)
  val emptySquare = topBottom(fff, fhf, fff)

  def showEmptyGrid(hole: => String, width: Width, height: Height): String = {
    val leftBorder = "<!"
    val rightBorder = "!>"
    val emptyRow = List.fill(width.value)(hole).mkString(leftBorder, "", rightBorder)
    val bottomBorder = List.fill(width.value)("==").mkString(leftBorder, "", rightBorder) ++
      "\n" ++ List.fill(width.value)("\\/").mkString("  ", "", "  ")
    (List.fill(height.value)(emptyRow) :+ bottomBorder).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    println(showEmptyGrid(hole = " .", Width(10), Height(20)))

    println("\n---")

    println(
      allTetrominoes
        .concat(List(plus, times, diamond, emptySquare))
        .toList
        .map(_.show(filled = _ => "🟩", hole = "⬜️"))
        .mkString("\n", "\n\n---\n\n", "\n")
    )
  }

}
