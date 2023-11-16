package adventofcode22

import cats._
import cats.implicits._
import scala.math.Numeric.Implicits._
import Day7.Path._
import Day7.FileSystem._
import Day7.TerminalInfo._
import Day7.TerminalLsLog._
import Day7.TerminalOutput._
import Stack._

object Day7 {

  case class FileName(value: String)

  case class DirName(value: String)
  object DirName {
    val root: DirName = DirName("/")
  }

  sealed trait Path {
    def `/`(dirName: DirName): Path = PathCons(path = this, dirName)

    def getDirName: DirName = this match {
      case Path.Root            => DirName.root
      case PathCons(_, dirName) => dirName
    }
  }
  object Path {
    case object Root extends Path
    case class PathCons(path: Path, dirName: DirName) extends Path

    def from(stack: Stack[DirName]): Option[Path] =
      stack.toList.reverse match {
        case DirName.root :: tail => Some(tail.foldLeft[Path](Root)(_ / _))
        case _                    => None
      }
  }

  case class Size(value: Long)
  object Size {
    implicit val order: Order[Size] = Order.by(_.value)
    implicit val monoid: Monoid[Size] = derived.semiauto.monoid
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
      def from(c: CdCmdTermOut): CdCmd = CdCmd(c.dirName)
    }
    case class CdUpCmd() extends TerminalInfo
    object CdUpCmd {
      def from(c: CdUpCmdTermOut): CdUpCmd = CdUpCmd()
    }
    case class Ls(logs: List[TerminalLsLog]) extends TerminalInfo {
      def `:+`(termLsLog: TerminalLsLog): Ls = Ls(logs :+ termLsLog)
    }
    object Ls {
      def from(l: LsCmdTermOut): Ls = Ls(List.empty)
    }
  }

  sealed trait FileSystem[A]
  object FileSystem {
    case class File[A](name: FileName, metadata: A) extends FileSystem[A]
    case class Dir[A](name: DirName, content: List[FileSystem[A]]) extends FileSystem[A]

    implicit val foldable: Foldable[FileSystem] = derived.semiauto.foldable
  }

  def getTerminalOutputs(input: List[String]): Option[List[TerminalOutput]] = input.traverse(TerminalOutput.from)

  def getTerminalInfos(terminalOutputs: TerminalOutput*): Option[List[TerminalInfo]] =
    terminalOutputs
      .foldLeft(List.empty[Option[TerminalInfo]]) { case (acc, termOut) =>
        def getNewAccAux(newLsLog: TerminalLsLog): List[Option[TerminalInfo]] =
          acc.lastOption
            .flatMap[Option[TerminalInfo]] { last =>
              last.map {
                case CdCmd(_) | CdUpCmd() => None
                case ls @ Ls(_)           => Some(ls :+ newLsLog)
              }
            }
            .flatTraverse(newLast => acc.dropRight(1) :+ newLast)

        termOut match {
          case c @ CdCmdTermOut(_)     => acc :+ Some(CdCmd.from(c))
          case c @ CdUpCmdTermOut()    => acc :+ Some(CdUpCmd.from(c))
          case l @ LsCmdTermOut()      => acc :+ Some(Ls.from(l))
          case dto @ DirTermOut(_)     => getNewAccAux(newLsLog = DirLsLog.from(dto))
          case fto @ FileTermOut(_, _) => getNewAccAux(newLsLog = FileLsLog.from(fto))
        }
      }
      .sequence

  type LsLogsByPath = Map[Path, List[TerminalLsLog]]

  def getLsLogsByPath(terminalInfos: TerminalInfo*): LsLogsByPath =
    terminalInfos
      .foldLeft[(Stack[DirName], LsLogsByPath)]((Stack.empty, Map.empty)) { case ((dirStack, lsLogsByPath), termInfo) =>
        termInfo match {
          case CdCmd(dirName) =>
            val newDirStack = dirStack.push(dirName)
            val newLsLogsByPath =
              Path
                .from(newDirStack)
                .map(newPath =>
                  lsLogsByPath
                    .get(newPath)
                    .map(_ => lsLogsByPath)
                    .getOrElse(lsLogsByPath.updated(newPath, List.empty))
                )
                .getOrElse(lsLogsByPath) // ignoring Path.from(dirStack) == None
            (newDirStack, newLsLogsByPath)
          case CdUpCmd() =>
            val newDirStack = dirStack.pop.fold(ifEmpty = dirStack)(_._2) // ignoring empty Stack[DirName]
            (newDirStack, lsLogsByPath)
          case Ls(logs) =>
            val newLsLogsByPath =
              Path
                .from(dirStack)
                .map(currentPath => lsLogsByPath.updated(currentPath, logs)) // overriding any pre-calculated content
                .getOrElse(lsLogsByPath) // ignoring Path.from(dirStack) == None
            (dirStack, newLsLogsByPath)
        }
      }
      ._2

  def getFileSystem(terminalInfos: TerminalInfo*): Option[FileSystem[Size]] = {
    val lsLogsByPath = getLsLogsByPath(terminalInfos: _*)
    def getFileSystemFromPath(path: Path): Option[FileSystem[Size]] =
      lsLogsByPath
        .getOrElse(path, List.empty)
        .traverse {
          case DirLsLog(dirName)         => getFileSystemFromPath(path / dirName)
          case FileLsLog(fileName, size) => Some(File(fileName, size))
        }
        .map(content => Dir(path.getDirName, content))
    getFileSystemFromPath(Path.Root)
  }

  def getAllDirSizes(fs: FileSystem[Size]): List[Size] = fs match {
    case File(_, _)      => List.empty
    case Dir(_, content) => fs.fold :: content.flatMap(getAllDirSizes)
  }

  def getAllDirSizesAtMost(maxSize: Size, fs: FileSystem[Size]): List[Size] =
    getAllDirSizes(fs).filter(_ <= maxSize)

  def getSumOfAllDirSizesAtMost100k(input: List[String]): Option[Size] =
    getTerminalOutputs(input)
      .flatMap(getTerminalInfos)
      .flatMap(getFileSystem)
      .map(fs => getAllDirSizesAtMost(maxSize = Size(100_000), fs).sum)

  val TOTAL_DISK_SPACE: Size = Size(70_000_000)
  val REQUIRED_UNUSED_DISK_SPACE: Size = Size(30_000_000)

  def getUnusedDiskSpace(fs: FileSystem[Size]): Size = TOTAL_DISK_SPACE - fs.fold

  def getSmallestDirSizeToDelete(fs: FileSystem[Size]): Option[Size] = {
    val unusedDiskSpace = getUnusedDiskSpace(fs)
    getAllDirSizes(fs).filter(_ >= REQUIRED_UNUSED_DISK_SPACE - unusedDiskSpace).sorted.headOption
  }

  def getSmallestDirSizeToDelete(input: List[String]): Option[Size] =
    getTerminalOutputs(input)
      .flatMap(getTerminalInfos)
      .flatMap(getFileSystem)
      .flatMap(getSmallestDirSizeToDelete)

}
