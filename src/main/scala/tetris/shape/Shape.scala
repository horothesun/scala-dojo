package tetris.shape

import cats._
import cats.implicits._
import scala.annotation.tailrec
import scala.math._
import scala.math.Numeric.Implicits._
import Models._
import Shape._

sealed trait Shape[A] {

  val width: Width
  val height: Height
  lazy val rasterized: Raster[A] = Shape.rasterized(this)

  def hFlipped: Shape[A] = HFlipped(this)
  def vFlipped: Shape[A] = VFlipped(this)
  def transposed: Shape[A] = Transposed(this)

  def rotatedCW: Shape[A] = transposed.vFlipped
  def rotatedCCW: Shape[A] = transposed.hFlipped

  def hRepeated(n: Int): Shape[A] = hStack(List.fill(n)(this))
  def vRepeated(n: Int): Shape[A] = vStack(List.fill(n)(this))

  def leftFilledBordered(a: A): Shape[A] = hStack(filled(a).vRepeated(height.value), this)
  def rightFilledBordered(a: A): Shape[A] = hStack(this, filled(a).vRepeated(height.value))
  def topFilledBordered(a: A): Shape[A] = vStack(filled(a).hRepeated(width.value), this)
  def bottomFilledBordered(a: A): Shape[A] = vStack(this, filled(a).hRepeated(width.value))
  def filledBordered(a: A): Shape[A] =
    if (isEmpty) filled(a).hRepeated(2).vRepeated(2)
    else leftFilledBordered(a).rightFilledBordered(a).topFilledBordered(a).bottomFilledBordered(a)

  def leftHoleBordered: Shape[A] = hStack(hole[A], this)
  def rightHoleBordered: Shape[A] = hStack(this, hole[A])
  def topHoleBordered: Shape[A] = vStack(hole[A], this)
  def bottomHoleBordered: Shape[A] = vStack(this, hole[A])
  def holeBordered: Shape[A] =
    if (isEmpty) hole[A].hRepeated(2).vRepeated(2)
    else leftHoleBordered.rightHoleBordered.topHoleBordered.bottomHoleBordered

  def inverted(ifHole: A): Shape[A] = Inverted(ifHole, this)

  def splittedByFilledRows: List[Shape[A]] = splittedByValidRows(validatedAllFilledRow, this)
  def splittedByFilledColumns: List[Shape[A]] = rotatedCW.splittedByFilledRows.map(_.rotatedCCW)

  def splittedByHoleRows: List[Shape[A]] = splittedByValidRows(validatedAllHoleRow, this)
  def splittedByHoleColumns: List[Shape[A]] = rotatedCW.splittedByHoleRows.map(_.rotatedCCW)

  def hHoleTrimmed: HTrimmed[A] = {
    val vTrimmed = rotatedCW.vHoleTrimmed
    HTrimmed(
      left = Width(vTrimmed.top.value),
      trimmed = vTrimmed.trimmed.rotatedCCW,
      right = Width(vTrimmed.bottom.value)
    )
  }
  def vHoleTrimmed: VTrimmed[A] = {
    def topHeightAndTrimmed(rowSplit: List[Shape[A]]): (Height, List[Shape[A]]) =
      rowSplit match {
        case Nil      => (Height(0), Nil)
        case ts :: ss => ts.validatedAllHole.fold(ifEmpty = (Height(0), rowSplit))(_ => (ts.height, ss))
      }
    def bottomTrimmedAndHeight(rowSplit: List[Shape[A]]): (List[Shape[A]], Height) = {
      val (bottom, bTrimmedReversed) = topHeightAndTrimmed(rowSplit.reverse)
      (bTrimmedReversed.reverse, bottom)
    }
    val (top, tTrimmed) = topHeightAndTrimmed(splittedByHoleRows)
    val (trimmed, bottom) = bottomTrimmedAndHeight(tTrimmed)
    VTrimmed(top, vStack(trimmed), bottom)
  }
  def holeTrimmed: Trimmed[A] = {
    val ht = hHoleTrimmed
    val vt = ht.trimmed.vHoleTrimmed
    Trimmed(top = vt.top, bottom = vt.bottom, left = ht.left, right = ht.right, trimmed = vt.trimmed)
  }

  def inFrontOf(that: Shape[A]): Shape[A] = {
    def adjusted(r: Width, b: Height): ShapeEndo[A] =
      repeat[A](max(0, r.value), _.bottomHoleBordered)
        .compose(repeat(max(0, b.value), _.rightHoleBordered))
    val rightFrontToBack = that.width - width
    val bottomFrontToBack = that.height - height
    val adjustedFront = adjusted(rightFrontToBack, bottomFrontToBack)(this)
    val adjustedBack = adjusted(-rightFrontToBack, -bottomFrontToBack)(that)
    val rows = adjustedFront.rasterized.value
      .zip(adjustedBack.rasterized.value)
      .map { case (fr, br) => fr.zip(br).map { case (f, b) => f.orElse(b) } }
    fromRaster(Raster(rows))
  }

  def mergedWith(that: Shape[A]): Option[Shape[A]] =
    Some((this, that)).filter { case (s1, s2) =>
      s1.width == s2.width && s1.height == s2.height
    }.mapFilter { case (s1, s2) =>
      s1.rasterized.value
        .zip(s2.rasterized.value)
        .traverse { case (r1, r2) => r1.zip(r2).traverse((atLeastOneNone[A] _).tupled) }
    }.map(rows => fromRaster(Raster(rows)))

  def validatedAllFilled: Option[Shape[A]] = rasterized.value.traverse(validatedAllFilledRow).as(this)
  def validatedAllHole: Option[Shape[A]] = rasterized.value.traverse(validatedAllHoleRow).as(this)

  def isEmpty: Boolean = width.value < 1 || height.value < 1
  def nonEmpty: Boolean = !isEmpty

  def map[B](f: A => B): Shape[B] = Functor[Shape].map(this)(f)

  def show(filled: A => String, hole: => String): String =
    rasterized.value.map(_.map(_.fold(ifEmpty = hole)(filled)).mkString("")).mkString("\n")

}
object Shape {

  case class Empty[A]() extends Shape[A] {
    override lazy val width: Width = Width(0)
    override lazy val height: Height = Height(0)
  }
  case class Hole[A]() extends Shape[A] {
    override lazy val width: Width = Width(1)
    override lazy val height: Height = Height(1)
  }
  case class Filled[A](a: A) extends Shape[A] {
    override lazy val width: Width = Width(1)
    override lazy val height: Height = Height(1)
  }
  case class HFlipped[A](s: Shape[A]) extends Shape[A] {
    override lazy val width: Width = s.width
    override lazy val height: Height = s.height
  }
  case class VFlipped[A](s: Shape[A]) extends Shape[A] {
    override lazy val width: Width = s.width
    override lazy val height: Height = s.height
  }
  case class Transposed[A](s: Shape[A]) extends Shape[A] {
    override lazy val width: Width = Width(s.height.value)
    override lazy val height: Height = Height(s.width.value)
  }
  case class HStack[A](ss: List[Shape[A]]) extends Shape[A] {
    override lazy val width: Width = ss.map(_.width).sum
    override lazy val height: Height = ss.map(_.height).max
  }
  // TODO: implement for performance reasons!!! 🔥🔥🔥
//    case class VStack[A](ss: List[Shape[A]]) extends Shape[A] {
//      override lazy val width: Width = ss.map(_.width).max
//      override lazy val height: Height = ss.map(_.height).sum
//    }
  case class Inverted[A](ifHole: A, s: Shape[A]) extends Shape[A] {
    override lazy val width: Width = s.width
    override lazy val height: Height = s.height
  }

  def rasterized[A]: Shape[A] => Raster[A] = {
    case Empty()       => Raster(List.empty)
    case Hole()        => Raster(List(List(None)))
    case Filled(a)     => Raster(List(List(Some(a))))
    case HFlipped(s)   => Raster(rasterized(s).value.reverse)
    case VFlipped(s)   => Raster(rasterized(s).value.map(_.reverse))
    case Transposed(s) => Raster(transpose(rasterized(s).value))
    case HStack(ss)    => Foldable[List].foldK(ss.map(rasterized))
    case Inverted(ifHole, s) =>
      Raster(rasterized(s).value.map(r => r.map(_.fold[Option[A]](ifEmpty = Some(ifHole))(_ => None))))
  }

  def empty[A]: Shape[A] = Empty()
  def hole[A]: Shape[A] = Hole()
  def filled[A](a: A): Shape[A] = Filled(a)

  def fromRaster[A](r: Raster[A]): Shape[A] =
    vStack(r.value.map(row => hStack(row.map(oa => oa.fold[Shape[A]](ifEmpty = Hole())(Filled.apply)))))

  def hStack[A](l: Shape[A], rs: Shape[A]*): Shape[A] = HStack(l :: rs.toList)
  def hStack[A](ss: List[Shape[A]]): Shape[A] = HStack(ss)

  // TODO: modify when VStack is implemented!!! 🔥🔥🔥
  def vStack[A](t: Shape[A], bs: Shape[A]*): Shape[A] = vStack(t :: bs.toList)
  def vStack[A](ss: List[Shape[A]]): Shape[A] = ss.fold[Shape[A]](Empty())(vStack(_: Shape[A], _: Shape[A]))
  def vStack[A](t: Shape[A], b: Shape[A]): Shape[A] = hStack(b.rotatedCW, t.rotatedCW).rotatedCCW

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

  def validatedAllFilledRow[A](r: Row[A]): Option[Row[A]] = (r: List[Option[A]]).sequence.as(r)
  def validatedAllHoleRow[A](r: Row[A]): Option[Row[A]] = Some(r).filter(_.forall(_.isEmpty)).as(r)

  def splittedByValidRows[A](validatedRow: Row[A] => Option[Row[A]], s: Shape[A]): List[Shape[A]] =
    s.rasterized.value
      // TODO: optimise for `::`!!! 🔥🔥🔥
      .foldLeft(List.empty[(Boolean, Raster[A])]) { case (acc, row) =>
        val rowIsValid = validatedRow(row).isDefined
        val newRowOnlyAcc = List((rowIsValid, Raster(List(row))))
        acc.toNel.fold(ifEmpty = newRowOnlyAcc) { accNel =>
          val (lastIsValid, lastRaster) = accNel.last
          if (rowIsValid == lastIsValid) acc.dropRight(1) :+ (lastIsValid, lastRaster :+ row)
          else acc ++ newRowOnlyAcc
        }
      }
      .map { case (_, r) => fromRaster(r) }

  def atLeastOneNone[A](o1: Option[A], o2: Option[A]): Option[Option[A]] =
    (o1, o2) match {
      case (Some(_), Some(_))             => None
      case (None, Some(_))                => Some(o2)
      case (Some(_), None) | (None, None) => Some(o1)
    }

  type ShapeEndo[A] = Shape[A] => Shape[A]
  implicit def shapeEndoMonoid[A]: Monoid[ShapeEndo[A]] = new Monoid[ShapeEndo[A]] {
    override def empty: ShapeEndo[A] = identity
    override def combine(f: ShapeEndo[A], g: ShapeEndo[A]): ShapeEndo[A] = f.compose(g)
  }

  def repeat[A](n: Int, se: ShapeEndo[A]): ShapeEndo[A] = Foldable[List].fold[ShapeEndo[A]](List.fill(n)(se))

  implicit val functor: Functor[Shape] = new Functor[Shape] {
    override def map[A, B](fa: Shape[A])(f: A => B): Shape[B] =
      fa match {
        case Empty()             => Empty[B]()
        case Hole()              => Hole[B]()
        case Filled(a)           => Filled(f(a))
        case HFlipped(s)         => HFlipped(map(s)(f))
        case VFlipped(s)         => VFlipped(map(s)(f))
        case Transposed(s)       => Transposed(map(s)(f))
        case HStack(ss)          => HStack(ss.map(s => map(s)(f)))
        case Inverted(ifHole, s) => Inverted(f(ifHole), map(s)(f))
      }
  }

  // TODO: implement (if possible)!!! 🔥🔥🔥
  implicit val applicative: Applicative[Shape] = new Applicative[Shape] {
    override def pure[A](x: A): Shape[A] = Filled(x)
    override def ap[A, B](ff: Shape[A => B])(fa: Shape[A]): Shape[B] =
      fa match {
        case Empty()             => Empty[B]()
        case Hole()              => Hole[B]()
        case Filled(a)           => ???
        case HFlipped(s)         => ???
        case VFlipped(s)         => ???
        case Transposed(s)       => ???
        case HStack(ss)          => ???
        case Inverted(ifHole, s) => ???
      }
  }

}
