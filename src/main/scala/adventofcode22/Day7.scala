package adventofcode22

import cats._
import cats.derived._
import Day7.FileSystem._

object Day7 {

  case class Name(value: String)

  case class Size(value: Int)
  object Size {
    implicit val numeric: Numeric[Size] = new Numeric[Size] {
      override def plus(x: Size, y: Size): Size = Size(Numeric[Int].plus(x.value, y.value))
      override def minus(x: Size, y: Size): Size = Size(Numeric[Int].minus(x.value, y.value))
      override def times(x: Size, y: Size): Size = Size(Numeric[Int].times(x.value, y.value))
      override def negate(x: Size): Size = Size(Numeric[Int].negate(x.value))
      override def fromInt(x: Int): Size = Size(x)
      override def parseString(str: String): Option[Size] = str.toIntOption.map(Size.apply)
      override def toInt(x: Size): Int = x.value
      override def toLong(x: Size): Long = x.value.toLong
      override def toFloat(x: Size): Float = x.value.toFloat
      override def toDouble(x: Size): Double = x.value.toDouble
      override def compare(x: Size, y: Size): Int = Numeric[Int].compare(x.value, y.value)
    }

    implicit val showPretty: ShowPretty[Size] = derived.semiauto.showPretty
  }

  sealed trait FileSystem[A] {
    def show(implicit sa: Show[A]): String = Show[FileSystem[A]].show(this)
    def showPretty(implicit spa: ShowPretty[A]): String = ShowPretty[FileSystem[A]].show(this)
  }
  object FileSystem {
    case class File[A](name: Name, a: A) extends FileSystem[A]
    case class Dir[A](name: Name, content: List[FileSystem[A]]) extends FileSystem[A]

    implicit val foldable: Foldable[FileSystem] = derived.semiauto.foldable
    implicit def show[A: Show]: Show[FileSystem[A]] = derived.semiauto.show
    implicit def showPretty[A: ShowPretty]: ShowPretty[FileSystem[A]] = derived.semiauto.showPretty
  }

}
