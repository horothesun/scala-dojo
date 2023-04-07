package tetris.shape

import cats.implicits._

object Models {

  type Row[A] = List[Option[A]]

  case class Width(value: Int) extends AnyVal {
    def `+`(that: Width): Width = Numeric[Width].plus(this, that)
    def `-`(that: Width): Width = Numeric[Width].minus(this, that)
    def `-` : Width = Numeric[Width].negate(this)
  }
  object Width {
    implicit val ordering: Ordering[Width] = Ordering[Int].contramap(_.value)
    implicit val numeric: Numeric[Width] = numericInstance[Width, Int](Width.apply, _.value)
  }

  case class Height(value: Int) extends AnyVal {
    def `-`(that: Height): Height = Numeric[Height].minus(this, that)
    def `-` : Height = Numeric[Height].negate(this)
    def `>=`(that: Height): Boolean = this.value >= that.value
  }
  object Height {
    implicit val ordering: Ordering[Height] = Ordering[Int].contramap(_.value)
    implicit val numeric: Numeric[Height] = numericInstance[Height, Int](Height.apply, _.value)
  }

  case class HTrimmed[A](left: Width, trimmed: Shape[A], right: Width)
  case class VTrimmed[A](top: Height, trimmed: Shape[A], bottom: Height)
  case class Trimmed[A](top: Height, bottom: Height, left: Width, right: Width, trimmed: Shape[A])

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

}
