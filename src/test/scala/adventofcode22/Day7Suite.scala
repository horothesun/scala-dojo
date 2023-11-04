package adventofcode22

import cats.implicits._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import Day7._
import Day7.FileSystem._
import Day7.TerminalInfo._
import Day7.TerminalLsLog._
import Day7.TerminalOutput._
import Day7Suite._

class Day7Suite extends ScalaCheckSuite {

  test("Stack.empty[Int].push(1).push(2).push(3).toList returns List(3, 2, 1)") {
    assertEquals(
      Stack.empty[Int].push(1).push(2).push(3).toList,
      List(3, 2, 1)
    )
  }

  test("Path.from(Stack.empty[DirName]) returns None") {
    assertEquals(Path.from(Stack.empty[DirName]), None)
  }

  test("Path.from returns None on Stack[DirName] representing 'a/b/c'") {
    val stack =
      Stack
        .empty[DirName]
        .push(DirName("a"))
        .push(DirName("b"))
        .push(DirName("c"))
    assertEquals(Path.from(stack), None)
  }

  property("Path.from returns None on Stack NOT starting with '/'") {
    forAll(dirNameStackNotStartingWithRoot) { stack =>
      assertEquals(Path.from(stack), None)
    }
  }

  test("Path.from returns correct value on Stack[DirName] representing '/a/b/c'") {
    val stack =
      Stack
        .empty[DirName]
        .push(DirName.root)
        .push(DirName("a"))
        .push(DirName("b"))
        .push(DirName("c"))
    assertEquals(
      Path.from(stack),
      Some(Path.Root / DirName("a") / DirName("b") / DirName("c"))
    )
  }

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

  test("getTerminalOutputs(bigInput) returns same-sized list") {
    assertEquals(getTerminalOutputs(bigInput).map(_.length), Some(bigInput.length))
  }

  test("getTerminalInfos on CdCmdTermOut followed by DirTermOut returns None") {
    assertEquals(
      getTerminalInfos(
        CdCmdTermOut(DirName.root),
        DirTermOut(DirName("a"))
      ),
      None
    )
  }

  test("getTerminalInfos on CdCmdTermOut followed by FileTermOut returns None") {
    assertEquals(
      getTerminalInfos(
        CdCmdTermOut(DirName.root),
        FileTermOut(FileName("b.txt"), Size(14848514))
      ),
      None
    )
  }

  test("getTerminalInfos on CdUpCmdTermOut followed by DirTermOut returns None") {
    assertEquals(
      getTerminalInfos(
        CdUpCmdTermOut(),
        DirTermOut(DirName("a"))
      ),
      None
    )
  }

  test("getTerminalInfos on CdUpCmdTermOut followed by FileTermOut returns None") {
    assertEquals(
      getTerminalInfos(
        CdUpCmdTermOut(),
        FileTermOut(FileName("b.txt"), Size(14848514))
      ),
      None
    )
  }

  test("getTerminalInfos returns valid value (small input)") {
    assertEquals(getTerminalInfos(terminalOutputs: _*), Some(terminalInfos))
  }

  test("getTerminalInfos(bigInput) preserves information") {
    val termOuts = getTerminalOutputs(bigInput)
    val termInfoSize = termOuts
      .flatMap(to => getTerminalInfos(to: _*))
      .map(tis =>
        tis.map {
          case CdCmd(_) | CdUpCmd() => 1
          case Ls(logs)             => 1 + logs.length
        }.sum
      )
    assertEquals(termInfoSize, Some(bigInput.length))
  }

  test("getFileSystem returns valid value") {
    assertEquals(getFileSystem(terminalInfos: _*), Some(root))
  }

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

  test("getAllDirSizesAtMost(maxSize = 100k, root) returns 'a' and 'e' directory sizes") {
    assertEquals(
      getAllDirSizesAtMost(maxSize = Size(100_000), root),
      List(Size(94853), Size(584))
    )
  }

  test("getSumOfAllDirSizesAtMost100k(bigInput) returns Size(1_611_443)") {
    assertEquals(getSumOfAllDirSizesAtMost100k(bigInput), Some(Size(1_611_443)))
  }

}
object Day7Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day7_input.txt")

  def stackGen[A](aGen: Gen[A]): Gen[Stack[A]] = Gen.lzy(Gen.oneOf(emptyStackGen[A], consStackGen(aGen)))
  def emptyStackGen[A]: Gen[Stack[A]] = Gen.const(Stack.empty[A])
  def consStackGen[A](aGen: Gen[A]): Gen[Stack[A]] = Gen.zip(aGen, stackGen(aGen)).map((Stack.Cons.apply[A] _).tupled)

  def nonRootDirNameGen: Gen[DirName] = Gen.stringOfN(3, Gen.alphaLowerChar).map(DirName.apply)
  def dirNameStackNotStartingWithRoot: Gen[Stack[DirName]] = stackGen[DirName](nonRootDirNameGen)

  val terminalOutputs: List[TerminalOutput] =
    List(
      CdCmdTermOut(DirName.root),
      LsCmdTermOut(),
      DirTermOut(DirName("a")),
      FileTermOut(FileName("b.txt"), Size(14848514)),
      FileTermOut(FileName("c.dat"), Size(8504156)),
      DirTermOut(DirName("d")),
      CdCmdTermOut(DirName("a")),
      LsCmdTermOut(),
      DirTermOut(DirName("e")),
      FileTermOut(FileName("f"), Size(29116)),
      FileTermOut(FileName("g"), Size(2557)),
      FileTermOut(FileName("h.lst"), Size(62596)),
      CdCmdTermOut(DirName("e")),
      LsCmdTermOut(),
      FileTermOut(FileName("i"), Size(584)),
      CdUpCmdTermOut(),
      CdUpCmdTermOut(),
      CdCmdTermOut(DirName("d")),
      LsCmdTermOut(),
      FileTermOut(FileName("j"), Size(4060174)),
      FileTermOut(FileName("d.log"), Size(8033020)),
      FileTermOut(FileName("d.ext"), Size(5626152)),
      FileTermOut(FileName("k"), Size(7214296))
    )

  val terminalInfos: List[TerminalInfo] =
    List(
      CdCmd(DirName.root),
      Ls(
        List(
          DirLsLog(DirName("a")),
          FileLsLog(FileName("b.txt"), Size(14848514)),
          FileLsLog(FileName("c.dat"), Size(8504156)),
          DirLsLog(DirName("d"))
        )
      ),
      CdCmd(DirName("a")),
      Ls(
        List(
          DirLsLog(DirName("e")),
          FileLsLog(FileName("f"), Size(29116)),
          FileLsLog(FileName("g"), Size(2557)),
          FileLsLog(FileName("h.lst"), Size(62596))
        )
      ),
      CdCmd(DirName("e")),
      Ls(
        List(
          FileLsLog(FileName("i"), Size(584))
        )
      ),
      CdUpCmd(),
      CdUpCmd(),
      CdCmd(DirName("d")),
      Ls(
        List(
          FileLsLog(FileName("j"), Size(4060174)),
          FileLsLog(FileName("d.log"), Size(8033020)),
          FileLsLog(FileName("d.ext"), Size(5626152)),
          FileLsLog(FileName("k"), Size(7214296))
        )
      )
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

  val root: FileSystem[Size] = Dir(DirName.root, List(a, b_txt, c_dat, d))

}
