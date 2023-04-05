import cats._
import cats.data.NonEmptyList
import cats.implicits._
import scala.annotation.tailrec
import scala.collection.immutable.Nil
import ClassicTetris.Color._
import ClassicTetris.MergedIntersection._
import ClassicTetris.Shape._
import ClassicTetris.Shape_._

object ClassicTetris {

  case class Width(value: Int) extends AnyVal {
    def `+`(that: Width): Width = Numeric[Width].plus(this, that)
  }
  object Width {
    implicit val ordering: Ordering[Width] = Ordering[Int].contramap(_.value)
    implicit val numeric: Numeric[Width] = numericInstance[Width, Int](Width.apply, _.value)
  }

  case class Height(value: Int) extends AnyVal {
    def `-`(that: Height): Height = Numeric[Height].minus(this, that)
    def `>=`(that: Height): Boolean = this.value >= that.value
  }
  object Height {
    implicit val ordering: Ordering[Height] = Ordering[Int].contramap(_.value)
    implicit val numeric: Numeric[Height] = numericInstance[Height, Int](Height.apply, _.value)
  }

  def numericInstance[W, V](from: V => W, value: W => V)(implicit numeric: Numeric[V]): Numeric[W] = new Numeric[W] {
    override def plus(x: W, y: W): W = from(Numeric[V].plus(value(x), value(y)))
    override def minus(x: W, y: W): W = from(Numeric[V].minus(value(x), value(y)))
    override def times(x: W, y: W): W = from(Numeric[V].times(value(x), value(y)))
    override def negate(x: W): W = from(Numeric[V].negate(value(x)))
    override def fromInt(x: Int): W = from(Numeric[V].fromInt(x))
    override def parseString(str: String): Option[W] = Numeric[V].parseString(str).map(from)
    override def toInt(x: W): Int = Numeric[V].toInt(value(x))
    override def toLong(x: W): Long = Numeric[V].toLong(value(x))
    override def toFloat(x: W): Float = Numeric[V].toFloat(value(x))
    override def toDouble(x: W): Double = Numeric[V].toDouble(value(x))
    override def compare(x: W, y: W): Int = Numeric[V].compare(value(x), value(y))
  }

  type Row[A] = List[Option[A]]
  type Raster[A] = List[Row[A]]

  def rasterWidth[A](r: Raster[A]): Width = r match {
    case Nil     => Width(0)
    case r1 :: _ => Width(r1.length)
  }
  def rasterHeight[A](r: Raster[A]): Height = Height(r.length)

  implicit val rasterFunctor: Functor[Raster] = new Functor[Raster] {
    override def map[A, B](fa: Raster[A])(f: A => B): Raster[B] = fa.map(_.map(_.map(f)))
  }

  implicit val rasterHorizontalMonoidK: MonoidK[Raster] = new MonoidK[Raster] {
    override def empty[A]: Raster[A] = List.empty

    override def combineK[A](x: Raster[A], y: Raster[A]): Raster[A] = {
      val renderedX =
        if (rasterHeight(x) >= rasterHeight(y)) x
        else {
          val numbOfMissingRows = (rasterHeight(y) - rasterHeight(x)).value
          val missingRow = List.fill[Option[A]](rasterWidth(x).value)(None)
          x ++ List.fill(numbOfMissingRows)(missingRow)
        }
      val renderedY =
        if (rasterHeight(y) >= rasterHeight(x)) y
        else {
          val numbOfMissingRows = (rasterHeight(x) - rasterHeight(y)).value
          val missingRow = List.fill[Option[A]](rasterWidth(y).value)(None)
          y ++ List.fill(numbOfMissingRows)(missingRow)
        }
      renderedX.zip(renderedY).map { case (xRow, yRow) => xRow ++ yRow }
    }
  }

  case class HTrimmed[A](left: Width, trimmed: Shape[A], right: Width)
  case class VTrimmed[A](top: Height, trimmed: Shape[A], bottom: Height)
  case class Trimmed[A](top: Height, bottom: Height, left: Width, right: Width, trimmed: Shape[A])

  sealed trait Shape_[A] {
    val width: Width
    val height: Height

    def hFlipped: Shape_[A] = HFlipped(this)
    def vFlipped: Shape_[A] = VFlipped(this)

    def transposed: Shape_[A] = Transposed(this)

    def rotatedCW: Shape_[A] = transposed.vFlipped
    def rotatedCCW: Shape_[A] = transposed.hFlipped

    def hRepeated(n: Int): Shape_[A] = hStack_(List.fill(n)(this))
    def vRepeated(n: Int): Shape_[A] = vStack_(List.fill(n)(this))

    def leftFilledBordered(a: A): Shape_[A] = hStack_(filled_(a).vRepeated(height.value), this)
    def rightFilledBordered(a: A): Shape_[A] = hStack_(this, filled_(a).vRepeated(height.value))
    def topFilledBordered(a: A): Shape_[A] = vStack_(filled_(a).hRepeated(width.value), this)
    def bottomFilledBordered(a: A): Shape_[A] = vStack_(this, filled_(a).hRepeated(width.value))
    def filledBordered(a: A): Shape_[A] =
      if (isEmpty) filled_(a).hRepeated(2).vRepeated(2)
      else leftFilledBordered(a).rightFilledBordered(a).topFilledBordered(a).bottomFilledBordered(a)

    def leftHoleBordered: Shape_[A] = hStack_(hole_[A], this)
    def rightHoleBordered: Shape_[A] = hStack_(this, hole_[A])
    def topHoleBordered: Shape_[A] = vStack_(hole_[A], this)
    def bottomHoleBordered: Shape_[A] = vStack_(this, hole_[A])
    def holeBordered: Shape_[A] =
      if (isEmpty) hole_[A].hRepeated(2).vRepeated(2)
      else leftHoleBordered.rightHoleBordered.topHoleBordered.bottomHoleBordered

    def isEmpty: Boolean = width.value < 1 || height.value < 1
    def nonEmpty: Boolean = !isEmpty
  }
  object Shape_ {
    case class Empty[A]() extends Shape_[A] {
      override lazy val width: Width = Width(0)
      override lazy val height: Height = Height(0)
    }
    case class Hole[A]() extends Shape_[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
    }
    case class Filled[A](a: A) extends Shape_[A] {
      override lazy val width: Width = Width(1)
      override lazy val height: Height = Height(1)
    }
    case class HFlipped[A](s: Shape_[A]) extends Shape_[A] {
      override lazy val width: Width = s.width
      override lazy val height: Height = s.height
    }
    case class VFlipped[A](s: Shape_[A]) extends Shape_[A] {
      override lazy val width: Width = s.width
      override lazy val height: Height = s.height
    }
    case class Transposed[A](s: Shape_[A]) extends Shape_[A] {
      override lazy val width: Width = Width(s.height.value)
      override lazy val height: Height = Height(s.width.value)
    }
    case class HStack[A](ss: List[Shape_[A]]) extends Shape_[A] {
      override lazy val width: Width = ss.map(_.width).sum
      override lazy val height: Height = ss.map(_.height).max
    }
    // TODO: implement for performance reasons!!! 🔥🔥🔥
//    case class VStack[A](ss: List[Shape_[A]]) extends Shape_[A] {
//      override lazy val width: Width = ss.map(_.width).max
//      override lazy val height: Height = ss.map(_.height).sum
//    }
    case class Inverted[A](ifHole: A, s: Shape_[A]) extends Shape_[A] {
      override lazy val width: Width = s.width
      override lazy val height: Height = s.height
    }
    // TODO: is this going to break the whole structure!?! 🔥🔥🔥
    case class Custom[A](r: Raster[A]) extends Shape_[A] {
      override lazy val width: Width = rasterWidth(r)
      override lazy val height: Height = rasterHeight(r)
    }

    def rasterized[A]: Shape_[A] => Raster[A] = {
      case Empty()             => List.empty
      case Hole()              => List(List(None))
      case Filled(a)           => List(List(Some(a)))
      case HFlipped(s)         => rasterized(s).reverse
      case VFlipped(s)         => rasterized(s).map(_.reverse)
      case Transposed(s)       => transpose(rasterized(s))
      case HStack(ss)          => Foldable[List].foldK(ss.map(rasterized))
      case Inverted(ifHole, s) => rasterized(s).map(r => r.map(_.fold[Option[A]](ifEmpty = Some(ifHole))(_ => None)))
      case Custom(r)           => r
    }

    def empty_[A]: Shape_[A] = Empty()
    def hole_[A]: Shape_[A] = Hole()
    def filled_[A](a: A): Shape_[A] = Filled(a)

    def hStack_[A](l: Shape_[A], rs: Shape_[A]*): Shape_[A] = HStack(l :: rs.toList)
    def hStack_[A](ss: List[Shape_[A]]): Shape_[A] = HStack(ss)

    // TODO: modify when VStack is implemented!!! 🔥🔥🔥
    def vStack_[A](t: Shape_[A], bs: Shape_[A]*): Shape_[A] = vStack_(t :: bs.toList)
    def vStack_[A](ss: List[Shape_[A]]): Shape_[A] = ss.fold[Shape_[A]](Empty())(vStack_(_: Shape_[A], _: Shape_[A]))
    def vStack_[A](t: Shape_[A], b: Shape_[A]): Shape_[A] = hStack_(b.rotatedCW, t.rotatedCW).rotatedCCW

    implicit val functor: Functor[Shape_] = new Functor[Shape_] {
      override def map[A, B](fa: Shape_[A])(f: A => B): Shape_[B] =
        fa match {
          case Empty()             => Empty[B]()
          case Hole()              => Hole[B]()
          case Filled(a)           => Filled(f(a))
          case HFlipped(s)         => HFlipped(map(s)(f))
          case VFlipped(s)         => VFlipped(map(s)(f))
          case Transposed(s)       => Transposed(map(s)(f))
          case HStack(ss)          => HStack(ss.map(s => map(s)(f)))
          case Inverted(ifHole, s) => Inverted(f(ifHole), map(s)(f))
          case Custom(r)           => Custom(Functor[Raster].map(r)(f))
        }
    }
  }

  trait Shape[A] {
    val width: Width
    val height: Height
    val rasterized: Raster[A]

    def hFlipped: Shape[A] = fromRasterUnsafe(rasterized.reverse)
    def vFlipped: Shape[A] = fromRasterUnsafe(rasterized.map(_.reverse))

    def transposed: Shape[A] = fromRasterUnsafe(transpose(rasterized))

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

    def inverted(ifHole: A): Shape[A] =
      fromRasterUnsafe(rasterized.map(r => r.map(_.fold[Option[A]](ifEmpty = Some(ifHole))(_ => None))))

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

    def mergedWith(that: Shape[A]): Option[Shape[A]] =
      Some((this, that)).filter { case (s1, s2) =>
        s1.width == s2.width && s1.height == s2.height
      }.mapFilter { case (s1, s2) =>
        s1.rasterized
          .zip(s2.rasterized)
          .traverse { case (r1, r2) => r1.zip(r2).traverse((atLeastOneNone[A] _).tupled) }
      }.mapFilter(fromRaster)

    def validatedAllFilled: Option[Shape[A]] = (rasterized: List[Row[A]]).traverse(validatedAllFilledRow).as(this)
    def validatedAllHole: Option[Shape[A]] = (rasterized: List[Row[A]]).traverse(validatedAllHoleRow).as(this)

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

    def fromRasterUnsafe[A](r: Raster[A]): Shape[A] = fromRaster(r).get
    def fromRaster[A](r: Raster[A]): Option[Shape[A]] =
      r match {
        case row :: _ if r.exists(_.length != row.length) => None
        case _ =>
          Some(new Shape[A] {
            override lazy val width: Width = r match {
              case Nil    => Width(0)
              case r :: _ => Width(r.length)
            }
            override lazy val height: Height = Height(r.length)
            override lazy val rasterized: Raster[A] = r
          })
      }

    def empty[A]: Shape[A] = MonoidK[Shape].empty
    def hole[A]: Shape[A] = fromRasterUnsafe(List(List(None)))
    def filled[A](a: A): Shape[A] = fromRasterUnsafe(List(List(Some(a))))

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

    def hStack[A](l: Shape[A], rs: Shape[A]*): Shape[A] = hStack(l :: rs.toList)
    def hStack[A](ss: List[Shape[A]]): Shape[A] = Foldable[List].foldK(ss)

    def vStack[A](t: Shape[A], bs: Shape[A]*): Shape[A] = vStack(t :: bs.toList)
    def vStack[A](ss: List[Shape[A]]): Shape[A] = ss.fold(MonoidK[Shape].empty)(vStack(_: Shape[A], _: Shape[A]))
    def vStack[A](t: Shape[A], b: Shape[A]): Shape[A] = hStack(b.rotatedCW, t.rotatedCW).rotatedCCW

    def validatedAllFilledRow[A](r: Row[A]): Option[Row[A]] = (r: List[Option[A]]).sequence.as(r)
    def validatedAllHoleRow[A](r: Row[A]): Option[Row[A]] = Some(r).filter(_.forall(_.isEmpty)).as(r)

    def splittedByValidRows[A](validatedRow: Row[A] => Option[Row[A]], s: Shape[A]): List[Shape[A]] =
      s.rasterized
        // TODO: optimise for `::`!!! 🔥🔥🔥
        .foldLeft(List.empty[(Boolean, Raster[A])]) { case (acc, r) =>
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

    def atLeastOneNone[A](o1: Option[A], o2: Option[A]): Option[Option[A]] =
      (o1, o2) match {
        case (Some(_), Some(_))             => None
        case (None, Some(_))                => Some(o2)
        case (Some(_), None) | (None, None) => Some(o1)
      }

    implicit val horizontalMonoidK: MonoidK[Shape] = new MonoidK[Shape] {
      override def empty[A]: Shape[A] = fromRasterUnsafe(List.empty)
      override def combineK[A](x: Shape[A], y: Shape[A]): Shape[A] = fromRasterUnsafe {
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

  // global coordinates
  case class Coord(x: Int, y: Int)

  sealed trait MergedIntersection[A] {
    def show(filled: A => String, hole: => String): String = this match {
      case NotIntersecting()             => "NotIntersecting"
      case ValidIntersection(mi)         => s"ValidIntersection(\n${mi.show(filled, hole)}\n)"
      case CollidingIntersection(i1, i2) => s"Colliding(\n${i1.show(filled, hole)}\n,\n${i2.show(filled, hole)}\n)"
    }
  }
  object MergedIntersection {
    case class NotIntersecting[A]() extends MergedIntersection[A]
    case class ValidIntersection[A](mergedIntersection: Shape[A]) extends MergedIntersection[A]
    case class CollidingIntersection[A](intersection1: Shape[A], intersection2: Shape[A]) extends MergedIntersection[A]
  }

  def mergedIntersection[A](bottomLeft1: Coord, s1: Shape[A], bottomLeft2: Coord, s2: Shape[A]): MergedIntersection[A] =
    if (s1.isEmpty || s2.isEmpty) ValidIntersection(empty[A])
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
    ).map { case (intersectionBottomLeft, intersectionTopRight) =>
      val windowed = windowedShape[A](intersectionBottomLeft, intersectionTopRight) _
      (windowed(bottomLeft1, s1), windowed(bottomLeft2, s2))
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
    bottomLeft: Coord, // x >= 0, y >= 0
    s: Shape[A]
  ): Shape[A] = {
    val localTop = (s.height.value - 1) - (windowTopRight.y - bottomLeft.y)
    val localBottom = (s.height.value - 1) - (windowBottomLeft.y - bottomLeft.y)
    val localRight = windowTopRight.x - bottomLeft.x
    val localLeft = windowBottomLeft.x - bottomLeft.x
    val localHeight = localBottom - localTop + 1
    val localWidth = localRight - localLeft + 1
    fromRasterUnsafe(
      s.rasterized
        .slice(localTop, localTop + localHeight)
        .map(r => r.slice(localLeft, localLeft + localWidth))
    )
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
      windowedShape(windowBottomLeft = Coord(x = 6, y = 3), windowTopRight = Coord(x = 7, y = 4))(
        bottomLeft = Coord(x = 5, y = 2),
        myShape02
      )
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
