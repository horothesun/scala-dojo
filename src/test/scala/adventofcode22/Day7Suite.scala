package adventofcode22

import cats.implicits._
import munit.ScalaCheckSuite
import Day7._
import Day7.FileSystem._
import Day7Suite._

class Day7Suite extends ScalaCheckSuite {

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

  val i: FileSystem[Size] = File(Name("i"), Size(584))
  val f: FileSystem[Size] = File(Name("f"), Size(29116))
  val g: FileSystem[Size] = File(Name("g"), Size(2557))
  val h_lst: FileSystem[Size] = File(Name("h.lst"), Size(62596))
  val b_txt: FileSystem[Size] = File(Name("b.txt"), Size(14848514))
  val c_dat: FileSystem[Size] = File(Name("c.dat"), Size(8504156))
  val j: FileSystem[Size] = File(Name("j"), Size(4060174))
  val d_log: FileSystem[Size] = File(Name("d.log"), Size(8033020))
  val d_ext: FileSystem[Size] = File(Name("d.ext"), Size(5626152))
  val k: FileSystem[Size] = File(Name("k"), Size(7214296))

  val e: FileSystem[Size] = Dir(Name("e"), List(i))
  val a: FileSystem[Size] = Dir(Name("a"), List(e, f, g, h_lst))
  val d: FileSystem[Size] = Dir(Name("d"), List(j, d_log, d_ext, k))

  val root: FileSystem[Size] = Dir(Name("/"), List(a, b_txt, c_dat, d))

}
