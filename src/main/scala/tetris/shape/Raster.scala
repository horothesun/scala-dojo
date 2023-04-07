package tetris.shape

import cats._
import Models._

case class Raster[A](value: List[Row[A]]) extends AnyVal {

  def width: Width = value match {
    case Nil     => Width(0)
    case r1 :: _ => Width(r1.length)
  }
  def height: Height = Height(value.length)

  def `:+`(row: Row[A]): Raster[A] = Raster(value :+ row)
}

object Raster {

  implicit val functor: Functor[Raster] = new Functor[Raster] {
    override def map[A, B](fa: Raster[A])(f: A => B): Raster[B] = Raster(fa.value.map(_.map(_.map(f))))
  }

  implicit val horizontalMonoidK: MonoidK[Raster] = new MonoidK[Raster] {
    override def empty[A]: Raster[A] = Raster(List.empty)
    override def combineK[A](x: Raster[A], y: Raster[A]): Raster[A] = {
      val rasterizedX =
        if (x.height >= y.height) x
        else {
          val numbOfMissingRows = (y.height - x.height).value
          val missingRow = List.fill[Option[A]](x.width.value)(None)
          Raster(x.value ++ List.fill(numbOfMissingRows)(missingRow))
        }
      val rasterizedY =
        if (y.height >= x.height) y
        else {
          val numbOfMissingRows = (x.height - y.height).value
          val missingRow = List.fill[Option[A]](y.width.value)(None)
          Raster(y.value ++ List.fill(numbOfMissingRows)(missingRow))
        }
      Raster(
        rasterizedX.value
          .zip(rasterizedY.value)
          .map { case (xRow, yRow) => xRow ++ yRow }
      )
    }
  }

}
