package adventofcode22

import cats.implicits._
import munit.ScalaCheckSuite
import Day7._
import Day7.FileSystem._
import Day7.TerminalOutput._
import Day7Suite._

class Day7Suite extends ScalaCheckSuite {

  test("getTerminalOutputs returns valid value") {
    val input =
      """
        |$ cd /
        |$ ls
        |dir a
        |14848514 b.txt
        |8504156 c.dat
        |dir d
        |$ cd a
        |$ ls
        |dir e
        |29116 f
        |2557 g
        |62596 h.lst
        |$ cd e
        |$ ls
        |584 i
        |$ cd ..
        |$ cd ..
        |$ cd d
        |$ ls
        |4060174 j
        |8033020 d.log
        |5626152 d.ext
        |7214296 k
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getTerminalOutputs(input), Some(terminalOutputs))
  }

//  test("getFileSystem returns valid value") {
//    assertEquals(getFileSystem(terminalOutputs), None)
//  }

  test("directory 'a' total size is 94853") {
    assertEquals(a.sumAll, Size(94853))
  }

  test("directory 'e' total size is 584") {
    assertEquals(e.sumAll, Size(584))
  }

  test("directory 'd' total size is 24933642") {
    assertEquals(d.sumAll, Size(24933642))
  }

  test("directory '/' total size is 48381165") {
    assertEquals(root.sumAll, Size(48381165))
  }

  test("getAllDirSizes(root) returns all directory sizes") {
    assertEquals(
      getAllDirSizes(root),
      List(Size(48381165), Size(94853), Size(584), Size(24933642))
    )
  }

  test("getAllDirSizesAtMost(maxSize = Size(100_000), root) returns 'a' and 'e' directory sizes") {
    assertEquals(
      getAllDirSizesAtMost(maxSize = Size(100_000), root),
      List(Size(94853), Size(584))
    )
  }

}
object Day7Suite {

  val terminalOutputs: List[TerminalOutput] =
    List(
      CdCmd(DirName("/")),
      LsCmd(),
      DirLog(DirName("a")),
      FileLog(FileName("b.txt"), Size(14848514)),
      FileLog(FileName("c.dat"), Size(8504156)),
      DirLog(DirName("d")),
      CdCmd(DirName("a")),
      LsCmd(),
      DirLog(DirName("e")),
      FileLog(FileName("f"), Size(29116)),
      FileLog(FileName("g"), Size(2557)),
      FileLog(FileName("h.lst"), Size(62596)),
      CdCmd(DirName("e")),
      LsCmd(),
      FileLog(FileName("i"), Size(584)),
      CdOutCmd(),
      CdOutCmd(),
      CdCmd(DirName("d")),
      LsCmd(),
      FileLog(FileName("j"), Size(4060174)),
      FileLog(FileName("d.log"), Size(8033020)),
      FileLog(FileName("d.ext"), Size(5626152)),
      FileLog(FileName("k"), Size(7214296))
    )

  val i: FileSystem[Size] = File(FileName("i"), Size(584))
  val f: FileSystem[Size] = File(FileName("f"), Size(29116))
  val g: FileSystem[Size] = File(FileName("g"), Size(2557))
  val h_lst: FileSystem[Size] = File(FileName("h.lst"), Size(62596))
  val b_txt: FileSystem[Size] = File(FileName("b.txt"), Size(14848514))
  val c_dat: FileSystem[Size] = File(FileName("c.dat"), Size(8504156))
  val j: FileSystem[Size] = File(FileName("j"), Size(4060174))
  val d_log: FileSystem[Size] = File(FileName("d.log"), Size(8033020))
  val d_ext: FileSystem[Size] = File(FileName("d.ext"), Size(5626152))
  val k: FileSystem[Size] = File(FileName("k"), Size(7214296))

  val e: FileSystem[Size] = Dir(DirName("e"), List(i))
  val a: FileSystem[Size] = Dir(DirName("a"), List(e, f, g, h_lst))
  val d: FileSystem[Size] = Dir(DirName("d"), List(j, d_log, d_ext, k))

  val root: FileSystem[Size] = Dir(DirName("/"), List(a, b_txt, c_dat, d))

}
