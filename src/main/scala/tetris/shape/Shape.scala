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

  def standardized: Shape[A] = fromRaster(rasterized)

  def hFlipped: Shape[A] = HFlipped(this)
  def vFlipped: Shape[A] = VFlipped(this)
  def transposed: Shape[A] = Transposed(this)

  def rotatedCW: Shape[A] = transposed.vFlipped
  def rotatedCCW: Shape[A] = transposed.hFlipped

  def hRepeated(n: Int): Shape[A] = hStack(List.fill(n)(this))
  def vRepeated(n: Int): Shape[A] = vStack(List.fill(n)(this))

  def leftFilledBordered(a: A, n: Int = 1): Shape[A] = hStack(List.fill(n)(filled(a).vRepeated(height.value)) :+ this)
  def rightFilledBordered(a: A, n: Int = 1): Shape[A] = hStack(this :: List.fill(n)(filled(a).vRepeated(height.value)))
  def topFilledBordered(a: A, n: Int = 1): Shape[A] = vStack(List.fill(n)(filled(a).hRepeated(width.value)) :+ this)
  def bottomFilledBordered(a: A, n: Int = 1): Shape[A] = vStack(this :: List.fill(n)(filled(a).hRepeated(width.value)))
  def filledBordered(a: A, n: Int = 1): Shape[A] =
    if (isEmpty)
      if (n < 1) empty[A] else filled(a).hRepeated(2).vRepeated(2).filledBordered(a, n - 1)
    else leftFilledBordered(a, n).rightFilledBordered(a, n).topFilledBordered(a, n).bottomFilledBordered(a, n)

  def leftHoleBordered(n: Int = 1): Shape[A] = hStack(List.fill(n)(hole[A]) :+ this)
  def rightHoleBordered(n: Int = 1): Shape[A] = hStack(this :: List.fill(n)(hole[A]))
  def topHoleBordered(n: Int = 1): Shape[A] = vStack(List.fill(n)(hole[A]) :+ this)
  def bottomHoleBordered(n: Int = 1): Shape[A] = vStack(this :: List.fill(n)(hole[A]))
  def holeBordered(n: Int = 1): Shape[A] =
    if (isEmpty)
      if (n < 1) empty[A] else hole[A].hRepeated(2).vRepeated(2).holeBordered(n - 1)
    else leftHoleBordered(n).rightHoleBordered(n).topHoleBordered(n).bottomHoleBordered(n)

  def inverted(ifHole: A): Shape[A] = Inverted(ifHole, this)

  def splitByFilledRows: List[Shape[A]] = splitByValidRows(validatedAllFilledRow, this)
  def splitByFilledColumns: List[Shape[A]] = rotatedCW.splitByFilledRows.map(_.rotatedCCW)

  def splitByHoleRows: List[Shape[A]] = splitByValidRows(validatedAllHoleRow, this)
  def splitByHoleColumns: List[Shape[A]] = rotatedCW.splitByHoleRows.map(_.rotatedCCW)

  def hSym: Shape[A] = hStack(this, vFlipped)
  def vSym: Shape[A] = vStack(this, hFlipped)
  def quarterSym: Shape[A] = hSym.vSym

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
    val (top, tTrimmed) = topHeightAndTrimmed(splitByHoleRows)
    val (trimmed, bottom) = bottomTrimmedAndHeight(tTrimmed)
    VTrimmed(top, vStack(trimmed), bottom)
  }
  def holeTrimmed: Trimmed[A] = {
    val ht = hHoleTrimmed
    val vt = ht.trimmed.vHoleTrimmed
    Trimmed(top = vt.top, bottom = vt.bottom, left = ht.left, right = ht.right, trimmed = vt.trimmed)
  }

  def above(that: Shape[A]): Shape[A] = {
    val rightFrontToBack = (that.width - width).value
    val bottomFrontToBack = (that.height - height).value
    val extendedFront = this.rightHoleBordered(max(0, rightFrontToBack)).bottomHoleBordered(max(0, bottomFrontToBack))
    val extendedBack = that.rightHoleBordered(max(0, -rightFrontToBack)).bottomHoleBordered(max(0, -bottomFrontToBack))
    val rows = extendedFront.rasterized.zip(extendedBack.rasterized).map(_.map { case (f, b) => f.orElse(b) })
    fromRaster(Raster(rows))
  }
  def below(that: Shape[A]): Shape[A] = that.above(this)

  def exclusivelyMergedWith(that: Shape[A]): Option[Shape[A]] =
    Some((this, that)).filter { case (s1, s2) =>
      s1.width == s2.width && s1.height == s2.height
    }.mapFilter { case (s1, s2) =>
      s1.rasterized.zip(s2.rasterized).traverse(_.traverse((atLeastOneNone[A] _).tupled))
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
    override lazy val height: Height = ss.map(_.height).maxOption.getOrElse(Height(0))
  }
  // TODO: implement for performance reasons!!! 🔥🔥🔥
//    case class VStack[A](ss: List[Shape[A]]) extends Shape[A] {
//      override lazy val width: Width = ss.map(_.width).maxOption.getOrElse(Width(0))
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

  def splitByValidRows[A](validatedRow: Row[A] => Option[Row[A]], s: Shape[A]): List[Shape[A]] =
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

  implicit def eq[A: Eq]: Eq[Shape[A]] = Eq.fromUniversalEquals

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

}
