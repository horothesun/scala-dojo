package tetris.shape

import cats.syntax.all._

object Models {

  type Row[A] = List[Option[A]]

  case class Width(value: Int) extends AnyVal
  object Width {
    implicit val ordering: Ordering[Width] = Ordering[Int].contramap(_.value)
    implicit val numeric: Numeric[Width] = Numeric[Int].imap(Width.apply)(_.value)
  }

  case class Height(value: Int) extends AnyVal
  object Height {
    implicit val ordering: Ordering[Height] = Ordering[Int].contramap(_.value)
    implicit val numeric: Numeric[Height] = Numeric[Int].imap(Height.apply)(_.value)
  }

  case class HTrimmed[A](left: Width, trimmed: Shape[A], right: Width)
  case class VTrimmed[A](top: Height, trimmed: Shape[A], bottom: Height)
  case class Trimmed[A](top: Height, bottom: Height, left: Width, right: Width, trimmed: Shape[A])

}
