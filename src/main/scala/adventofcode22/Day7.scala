package adventofcode22

import cats._
import cats.implicits._
import Day7.FileSystem._

object Day7 {

  case class Name(value: String)

  case class Size(value: Int)
  object Size {
    implicit val order: Order[Size] = Order.by(_.value)
    implicit val numeric: Numeric[Size] = Numeric[Int].imap(Size.apply)(_.value)
  }

  sealed trait FileSystem[A]
  object FileSystem {
    case class File[A](name: Name, metadata: A) extends FileSystem[A]
    case class Dir[A](name: Name, content: List[FileSystem[A]]) extends FileSystem[A]

    implicit val foldable: Foldable[FileSystem] = derived.semiauto.foldable
  }

  def getAllDirSizes(fs: FileSystem[Size]): List[Size] = fs match {
    case File(_, _)      => List.empty
    case Dir(_, content) => fs.sumAll :: content.flatMap(getAllDirSizes)
  }

  def getAllDirSizesAtMost(maxSize: Size, fs: FileSystem[Size]): List[Size] =
    getAllDirSizes(fs).filter(_ < maxSize)

}
