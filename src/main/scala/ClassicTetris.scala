import cats.MonoidK
import ClassicTetris.Color.Mono
import ClassicTetris.Shape._
import scala.annotation.tailrec
import scala.collection.immutable.Nil

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

  case class Dimensions(width: Width, height: Height)

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

  trait Shape[A] {
    val dims: Dimensions
    def render: List[List[Option[A]]]

    def show(filled: A => String, hole: => String): String =
      this.render.map(_.map(_.fold(ifEmpty = hole)(filled)).mkString("")).mkString("\n")
  }
  object Shape {

    def empty[A]: Shape[A] = MonoidK[Shape].empty
    def hole[A]: Shape[A] = new Shape[A] {
      override lazy val dims: Dimensions = Dimensions(Width(1), Height(1))
      override def render: List[List[Option[A]]] = List(List(None))
    }
    def filled[A](a: A): Shape[A] = new Shape[A] {
      override lazy val dims: Dimensions = Dimensions(Width(1), Height(1))
      override def render: List[List[Option[A]]] = List(List(Some(a)))
    }

    def rotatedCW[A](s: Shape[A]): Shape[A] = new Shape[A] {
      override lazy val dims: Dimensions = Dimensions(Width(s.dims.height.value), Height(s.dims.width.value))
      override def render: List[List[Option[A]]] = transpose(s.render).map(_.reverse).reverse
    }

    def beside[A](l: Shape[A], r: Shape[A]): Shape[A] = MonoidK[Shape].combineK(l, r)

    def beside[A](l: Shape[A], rs: Shape[A]*): Shape[A] = if (rs.isEmpty) l else rs.fold(l)(beside)

    def topBottom[A](t: Shape[A], b: Shape[A]): Shape[A] =
      rotatedCW(rotatedCW(rotatedCW(beside(rotatedCW(b), rotatedCW(t)))))

    def topBottom[A](t: Shape[A], bs: Shape[A]*): Shape[A] = if (bs.isEmpty) t else bs.fold(t)(topBottom)

    implicit val horizontalMonoidK: MonoidK[Shape] = new MonoidK[Shape] {
      override def empty[A]: Shape[A] = new Shape[A] {
        override lazy val dims: Dimensions = Dimensions(Width(0), Height(0))
        override def render: List[List[Option[A]]] = List.empty
      }
      override def combineK[A](x: Shape[A], y: Shape[A]): Shape[A] = new Shape[A] {
        override lazy val dims: Dimensions = Dimensions(
          x.dims.width + y.dims.width,
          Height.max(x.dims.height, y.dims.height)
        )
        override def render: List[List[Option[A]]] = {
          val renderedX =
            if (x.dims.height >= y.dims.height) x.render
            else {
              val numbOfMissingRows = (y.dims.height - x.dims.height).value
              val missingRow = List.fill[Option[A]](x.dims.width.value)(None)
              x.render ++ List.fill(numbOfMissingRows)(missingRow)
            }
          val renderedY =
            if (y.dims.height >= x.dims.height) y.render
            else {
              val numbOfMissingRows = (x.dims.height - y.dims.height).value
              val missingRow = List.fill[Option[A]](y.dims.width.value)(None)
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
  val hfh = beside(h, f, h)
  val hff = beside(h, f, f)
  val ffh = beside(f, f, h)
  val i = beside(f, f, f, f)
  val o = topBottom(
    ff,
    ff
  )
  val t = topBottom(
    fff,
    hfh
  )
  val j = topBottom(
    hf,
    hf,
    hf,
    ff
  )
  val l = topBottom(
    fh,
    fh,
    fh,
    ff
  )
  val s = topBottom(
    hff,
    ffh
  )
  val z = topBottom(
    ffh,
    hff
  )
  val plus = topBottom(
    hfh,
    fff,
    hfh
  )

  def showEmptyGrid(hole: => String, ds: Dimensions): String = {
    val leftBorder = "<!"
    val rightBorder = "!>"
    val emptyRow = List.fill(ds.width.value)(hole).mkString(leftBorder, "", rightBorder)
    val bottomBorder = List.fill(ds.width.value)("==").mkString(leftBorder, "", rightBorder) ++
      "\n" ++ List.fill(ds.width.value)("\\/").mkString("  ", "", "  ")
    (List.fill(ds.height.value)(emptyRow) :+ bottomBorder).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    println(showEmptyGrid(hole = " .", Dimensions(Width(10), Height(20))))

    println("\n---")

    println(
      List(i, o, t, j, l, s, z, plus)
        .map(_.show(filled = _ => "🟩", hole = "⬜️"))
        .mkString("\n", "\n\n---\n\n", "\n")
    )
  }

}
