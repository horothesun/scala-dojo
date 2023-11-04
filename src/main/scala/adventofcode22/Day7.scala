package adventofcode22

import cats._
import cats.implicits._
import Day7.FileSystem._
import Day7.Stack._
import Day7.TerminalInfo._
import Day7.TerminalLsLog._
import Day7.TerminalOutput._

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

    def empty[A]: Stack[A] = Empty()
  }

  case class FileName(value: String)

  case class DirName(value: String)
  object DirName {
    val root: DirName = DirName("/")
  }

  case class Size(value: Long)
  object Size {
    implicit val order: Order[Size] = Order.by(_.value)
    implicit val numeric: Numeric[Size] = Numeric[Long].imap(Size.apply)(_.value)
  }

  sealed trait TerminalOutput
  object TerminalOutput {
    case class CdCmdTermOut(dirName: DirName) extends TerminalOutput
    object CdCmdTermOut {
      def from(s: String): Option[CdCmdTermOut] = s.split(' ') match {
        case Array("$", "cd", dn) => Some(CdCmdTermOut(DirName(dn)))
        case _                    => None
      }
    }
    case class CdUpCmdTermOut() extends TerminalOutput
    object CdUpCmdTermOut {
      def from(s: String): Option[CdUpCmdTermOut] = s match {
        case "$ cd .." => Some(CdUpCmdTermOut())
        case _         => None
      }
    }
    case class LsCmdTermOut() extends TerminalOutput
    object LsCmdTermOut {
      def from(s: String): Option[LsCmdTermOut] = s.split(' ') match {
        case Array("$", "ls") => Some(LsCmdTermOut())
        case _                => None
      }
    }
    case class DirTermOut(dirName: DirName) extends TerminalOutput
    object DirTermOut {
      def from(s: String): Option[DirTermOut] = s.split(' ') match {
        case Array("dir", dn) => Some(DirTermOut(DirName(dn)))
        case _                => None
      }
    }
    case class FileTermOut(fileName: FileName, size: Size) extends TerminalOutput
    object FileTermOut {
      def from(s: String): Option[FileTermOut] = s.split(' ') match {
        case Array(size, fn) => size.toIntOption.map(sz => FileTermOut(FileName(fn), Size(sz)))
        case _               => None
      }
    }

    def from(s: String): Option[TerminalOutput] =
      List[String => Option[TerminalOutput]](
        CdUpCmdTermOut.from,
        CdCmdTermOut.from,
        LsCmdTermOut.from,
        DirTermOut.from,
        FileTermOut.from
      )
        .map(_(s))
        .reduce(_ orElse _)
  }

  sealed trait TerminalLsLog
  object TerminalLsLog {
    case class DirLsLog(dirName: DirName) extends TerminalLsLog
    object DirLsLog {
      def from(dto: DirTermOut): DirLsLog = DirLsLog(dto.dirName)
    }
    case class FileLsLog(fileName: FileName, size: Size) extends TerminalLsLog
    object FileLsLog {
      def from(fto: FileTermOut): FileLsLog = FileLsLog(fto.fileName, fto.size)
    }
  }

  sealed trait TerminalInfo
  object TerminalInfo {
    case class CdCmd(dirName: DirName) extends TerminalInfo
    object CdCmd {
      def from(ccto: CdCmdTermOut): CdCmd = CdCmd(ccto.dirName)
    }
    case class CdUpCmd() extends TerminalInfo
    case class Ls(logs: List[TerminalLsLog]) extends TerminalInfo {
      def `:+`(termLsLog: TerminalLsLog): Ls = Ls(logs :+ termLsLog)
    }
  }

  sealed trait FileSystem[A]
  object FileSystem {
    case class File[A](name: FileName, metadata: A) extends FileSystem[A]
    case class Dir[A](name: DirName, content: List[FileSystem[A]]) extends FileSystem[A]

    implicit val foldable: Foldable[FileSystem] = derived.semiauto.foldable
  }

  def getTerminalOutputs(input: List[String]): Option[List[TerminalOutput]] = input.traverse(TerminalOutput.from)

  // TODO: find structure to simplify this implementation 🔥🔥🔥
  def getTerminalInfos(terminalOutputs: TerminalOutput*): Option[List[TerminalInfo]] =
    terminalOutputs
      .foldLeft(List.empty[Option[TerminalInfo]]) { case (acc, termOut) =>
        def getNewAccAux(newLsLog: TerminalLsLog): List[Option[TerminalInfo]] =
          acc.lastOption
            .flatMap[Option[TerminalInfo]] { last =>
              last.map {
                case CdCmd(_)   => None
                case CdUpCmd()  => None
                case ls @ Ls(_) => Some(ls :+ newLsLog)
              }
            }
            .flatTraverse(newLast => acc.dropRight(1) :+ newLast)

        termOut match {
          case ccto @ CdCmdTermOut(_)  => acc :+ Some(CdCmd.from(ccto))
          case CdUpCmdTermOut()        => acc :+ Some(CdUpCmd())
          case LsCmdTermOut()          => acc :+ Some(Ls(List.empty))
          case dto @ DirTermOut(_)     => getNewAccAux(newLsLog = DirLsLog.from(dto))
          case fto @ FileTermOut(_, _) => getNewAccAux(newLsLog = FileLsLog.from(fto))
        }
      }
      .sequence

  def getFileSystem(terminalInfos: TerminalInfo*): Option[FileSystem[Size]] = {
    type LsLogsByDirName = Map[DirName, List[TerminalLsLog]]
    val (_, lsLogsByDirName) = terminalInfos.foldLeft[(Stack[DirName], LsLogsByDirName)]((Stack.empty, Map.empty)) {
      case ((dirStack, lsLogsByDirName), termInfo) =>
        termInfo match {
          case CdCmd(dirName) =>
            (
              dirStack.push(dirName),
              lsLogsByDirName
                .get(dirName)
                .fold(ifEmpty = lsLogsByDirName.updated(dirName, List.empty))(_ => lsLogsByDirName)
            )
          case CdUpCmd() =>
            (
              dirStack.pop.fold(ifEmpty = dirStack)(_._2), // TODO: handle empty Stack[DirName]!!! 🔥🔥🔥
              lsLogsByDirName
            )
          case Ls(logs) =>
            // TODO: handle empty Stack[DirName]!!! 🔥🔥🔥
            dirStack.peek.fold(ifEmpty = (dirStack, lsLogsByDirName)) { currentDir =>
              (
                dirStack,
                lsLogsByDirName.updated(currentDir, logs) // override any pre-calculated content
              )
            }
        }
    }

    def aux(dirName: DirName): Option[FileSystem[Size]] =
      lsLogsByDirName
        .getOrElse(dirName, List.empty)
        .traverse {
          case DirLsLog(dirName)         => aux(dirName)
          case FileLsLog(fileName, size) => Some(File(fileName, size))
        }
        .map(content => Dir(dirName, content))

    aux(DirName.root)
  }

  def getAllDirSizes(fs: FileSystem[Size]): List[Size] = fs match {
    case File(_, _)      => List.empty
    case Dir(_, content) => fs.sumAll :: content.flatMap(getAllDirSizes)
  }

  def getAllDirSizesAtMost(maxSize: Size, fs: FileSystem[Size]): List[Size] =
    getAllDirSizes(fs).filter(_ < maxSize)

  def getSumOfAllDirSizesAtMost100k(input: List[String]): Option[Size] =
    getTerminalOutputs(input)
      .flatMap(getTerminalInfos)
      .flatMap(getFileSystem)
      .map(fs => getAllDirSizesAtMost(maxSize = Size(100_000), fs).sum)

}
