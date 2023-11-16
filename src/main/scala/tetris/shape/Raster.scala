package tetris.shape

import cats._
import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._
import Models._


case class Raster[A](value: List[Row[A]]) extends AnyVal {

  def width: Width = value match {
    case Nil     => Width(0)
    case r1 :: _ => Width(r1.length)
  }
  def height: Height = Height(value.length)

  def `:+`(row: Row[A]): Raster[A] = Raster(value :+ row)

  def zip[B](that: Raster[B]): List[List[(Option[A], Option[B])]] =
    value.zip(that.value).map { case (r1, r2) => r1.zip(r2) }

  def map[B](f: A => B): Raster[B] = Functor[Raster].map(this)(f)

}

object Raster {

  implicit def eq[A: Eq]: Eq[Raster[A]] = Eq.fromUniversalEquals

  implicit val functor: Functor[Raster] = new Functor[Raster] {
    override def map[A, B](fa: Raster[A])(f: A => B): Raster[B] = Raster(fa.value.map(_.map(_.map(f))))
  }

  implicit val horizontalMonoidK: MonoidK[Raster] = new MonoidK[Raster] {
    override def empty[A]: Raster[A] = Raster(List.empty)
    override def combineK[A](r1: Raster[A], r2: Raster[A]): Raster[A] = {
      val value1 =
        if (r1.height >= r2.height) r1.value
        else {
          val numbOfMissingRows = (r2.height - r1.height).value
          val missingRow = List.fill[Option[A]](r1.width.value)(None)
          r1.value ++ List.fill(numbOfMissingRows)(missingRow)
        }
      val value2 =
        if (r2.height >= r1.height) r2.value
        else {
          val numbOfMissingRows = (r1.height - r2.height).value
          val missingRow = List.fill[Option[A]](r2.width.value)(None)
          r2.value ++ List.fill(numbOfMissingRows)(missingRow)
        }
      Raster(value1.zip(value2).map { case (row1, row2) => row1 ++ row2 })
    }
  }

}
