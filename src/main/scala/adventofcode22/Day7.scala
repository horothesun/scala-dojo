package adventofcode22

import cats._
import cats.implicits._
import Day7.FileSystem._
import Day7.Stack._

object Day7 {

  sealed trait Stack[A] {
    def push(a: A): Stack[A] = Cons(a, this)
    def pop: Option[(A, Stack[A])] = this match {
      case Empty()         => None
      case Cons(top, tail) => Some((top, tail))
    }
    def peek: Option[A] = this match {
      case Empty()      => None
      case Cons(top, _) => Some(top)
    }
  }
  object Stack {
    case class Empty[A]() extends Stack[A]
    case class Cons[A](top: A, tail: Stack[A]) extends Stack[A]

//    def apply[A](as: A*): Stack[A] = as.reverse.foldLeft[Stack[A]](Empty()) { case (s, a) => Cons(a, s) }
    def empty[A]: Stack[A] = Empty()
  }

  sealed trait TerminalOutput
  object TerminalOutput {
    case class CdCmd(dirName: DirName) extends TerminalOutput
    object CdCmd {
      def from(s: String): Option[CdCmd] = s.split(' ') match {
        case Array("$", "cd", dn) => Some(CdCmd(DirName(dn)))
        case _                    => None
      }
    }
    case class CdOutCmd() extends TerminalOutput
    object CdOutCmd {
      def from(s: String): Option[CdOutCmd] = s match {
        case "$ cd .." => Some(CdOutCmd())
        case _         => None
      }
    }
    case class LsCmd() extends TerminalOutput
    object LsCmd {
      def from(s: String): Option[LsCmd] = s.split(' ') match {
        case Array("$", "ls") => Some(LsCmd())
        case _                => None
      }
    }
    case class DirLog(dirName: DirName) extends TerminalOutput
    object DirLog {
      def from(s: String): Option[DirLog] = s.split(' ') match {
        case Array("dir", dn) => Some(DirLog(DirName(dn)))
        case _                => None
      }
    }
    case class FileLog(fileName: FileName, size: Size) extends TerminalOutput
    object FileLog {
      def from(s: String): Option[FileLog] = s.split(' ') match {
        case Array(size, fn) => size.toIntOption.map(sz => FileLog(FileName(fn), Size(sz)))
        case _               => None
      }
    }

    def from(s: String): Option[TerminalOutput] =
      List[String => Option[TerminalOutput]](
        CdOutCmd.from,
        CdCmd.from,
        LsCmd.from,
        DirLog.from,
        FileLog.from
      )
        .map(_(s))
        .reduce(_ orElse _)
  }

  def getTerminalOutputs(input: List[String]): Option[List[TerminalOutput]] =
    input.traverse(TerminalOutput.from)

  case class FileName(value: String)
  case class DirName(value: String)

  case class Size(value: Int)
  object Size {
    implicit val order: Order[Size] = Order.by(_.value)
    implicit val numeric: Numeric[Size] = Numeric[Int].imap(Size.apply)(_.value)
  }

  sealed trait FileSystem[A]
  object FileSystem {
    case class File[A](name: FileName, metadata: A) extends FileSystem[A]
    case class Dir[A](name: DirName, content: List[FileSystem[A]]) extends FileSystem[A]

    implicit val foldable: Foldable[FileSystem] = derived.semiauto.foldable
  }

  def getAllDirSizes(fs: FileSystem[Size]): List[Size] = fs match {
    case File(_, _)      => List.empty
    case Dir(_, content) => fs.sumAll :: content.flatMap(getAllDirSizes)
  }

  def getAllDirSizesAtMost(maxSize: Size, fs: FileSystem[Size]): List[Size] =
    getAllDirSizes(fs).filter(_ < maxSize)

}
