package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import Day8._
import Day8.TreeVisibility._
import Day8.Visibility._
import Day8Suite._

class Day8Suite extends ScalaCheckSuite {

  test("Tree.from('2') returns Some(Tree(Height(2)))") {
    assertEquals(Tree.from('2'), Some(Tree(Height(2))))
  }

  property("Tree.from(c) returns None when c is NOT a digit Char") {
    forAll(Gen.alphaChar)(c => assertEquals(Tree.from(c), None))
  }

  test("getForestFrom returns the correct Forest (small input)") {
    val input =
      """
        |30373
        |25512
        |65332
        |33549
        |35390
        |""".stripMargin.linesIterator.toList.drop(1)
    assertEquals(getForestFrom(input), Some(forest))
  }

  test("getVisibilityFromLeft(3,0,3,7,3) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(3)), Tree(Height(0)), Tree(Height(3)), Tree(Height(7)), Tree(Height(3)))
    assertEquals(getVisibilityFromLeft(row), NonEmptyList.of(Visible, Hidden, Hidden, Visible, Hidden))
  }

  test("getVisibilityFromLeft(2,5,5,1,2) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(2)), Tree(Height(5)), Tree(Height(5)), Tree(Height(1)), Tree(Height(2)))
    assertEquals(getVisibilityFromLeft(row), NonEmptyList.of(Visible, Visible, Hidden, Hidden, Hidden))
  }

  test("getVisibilityFromLeft(6,5,3,3,2) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(6)), Tree(Height(5)), Tree(Height(3)), Tree(Height(3)), Tree(Height(2)))
    assertEquals(getVisibilityFromLeft(row), NonEmptyList.of(Visible, Hidden, Hidden, Hidden, Hidden))
  }

  test("getVisibilityFromLeft(3,3,5,4,9) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(3)), Tree(Height(3)), Tree(Height(5)), Tree(Height(4)), Tree(Height(9)))
    assertEquals(getVisibilityFromLeft(row), NonEmptyList.of(Visible, Hidden, Visible, Hidden, Visible))
  }

  test("getVisibilityFromLeft(3,5,3,9,0) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(3)), Tree(Height(5)), Tree(Height(3)), Tree(Height(9)), Tree(Height(0)))
    assertEquals(getVisibilityFromLeft(row), NonEmptyList.of(Visible, Visible, Hidden, Visible, Hidden))
  }

  test("getVisibilityFromRight(3,0,3,7,3) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(3)), Tree(Height(0)), Tree(Height(3)), Tree(Height(7)), Tree(Height(3)))
    assertEquals(getVisibilityFromRight(row), NonEmptyList.of(Hidden, Hidden, Hidden, Visible, Visible))
  }

  test("getVisibilityFromRight(2,5,5,1,2) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(2)), Tree(Height(5)), Tree(Height(5)), Tree(Height(1)), Tree(Height(2)))
    assertEquals(getVisibilityFromRight(row), NonEmptyList.of(Hidden, Hidden, Visible, Hidden, Visible))
  }

  test("getVisibilityFromRight(6,5,3,3,2) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(6)), Tree(Height(5)), Tree(Height(3)), Tree(Height(3)), Tree(Height(2)))
    assertEquals(getVisibilityFromRight(row), NonEmptyList.of(Visible, Visible, Hidden, Visible, Visible))
  }

  test("getVisibilityFromRight(3,3,5,4,9) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(3)), Tree(Height(3)), Tree(Height(5)), Tree(Height(4)), Tree(Height(9)))
    assertEquals(getVisibilityFromRight(row), NonEmptyList.of(Hidden, Hidden, Hidden, Hidden, Visible))
  }

  test("getVisibilityFromRight(3,5,3,9,0) returns correct visibilities") {
    val row = NonEmptyList.of(Tree(Height(3)), Tree(Height(5)), Tree(Height(3)), Tree(Height(9)), Tree(Height(0)))
    assertEquals(getVisibilityFromRight(row), NonEmptyList.of(Hidden, Hidden, Hidden, Visible, Visible))
  }

  property("getVisibilityFromLeft(row) == getVisibilityFromRight(row.reverse).reverse") {
    forAll(treeRowGen) { row =>
      assertEquals(getVisibilityFromLeft(row), getVisibilityFromRight(row.reverse).reverse)
    }
  }

  property("getVisibilityFromRight(row) == getVisibilityFromLeft(row.reverse).reverse") {
    forAll(treeRowGen) { row =>
      assertEquals(getVisibilityFromRight(row), getVisibilityFromLeft(row.reverse).reverse)
    }
  }

  test("forest is correct") {
    assertEquals(
      forest,
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of(Tree(Height(3)), Tree(Height(0)), Tree(Height(3)), Tree(Height(7)), Tree(Height(3))),
          NonEmptyList.of(Tree(Height(2)), Tree(Height(5)), Tree(Height(5)), Tree(Height(1)), Tree(Height(2))),
          NonEmptyList.of(Tree(Height(6)), Tree(Height(5)), Tree(Height(3)), Tree(Height(3)), Tree(Height(2))),
          NonEmptyList.of(Tree(Height(3)), Tree(Height(3)), Tree(Height(5)), Tree(Height(4)), Tree(Height(9))),
          NonEmptyList.of(Tree(Height(3)), Tree(Height(5)), Tree(Height(3)), Tree(Height(9)), Tree(Height(0)))
        )
      )
    )
  }

  test("NonEmptyMatrix rotatedCW returns correct NonEmptyMatrix") {
    assertEquals(
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of(3, 0, 3, 7, 3),
          NonEmptyList.of(2, 5, 5, 1, 2),
          NonEmptyList.of(6, 5, 3, 3, 2),
          NonEmptyList.of(3, 3, 5, 4, 9),
          NonEmptyList.of(3, 5, 3, 9, 0)
        )
      ).rotatedCW,
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of(3, 3, 6, 2, 3),
          NonEmptyList.of(5, 3, 5, 5, 0),
          NonEmptyList.of(3, 5, 3, 5, 3),
          NonEmptyList.of(9, 4, 3, 1, 7),
          NonEmptyList.of(0, 9, 2, 2, 3)
        )
      )
    )
  }

  test("NonEmptyMatrix rotatedCCW returns correct NonEmptyMatrix") {
    assertEquals(
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of(3, 0, 3, 7, 3),
          NonEmptyList.of(2, 5, 5, 1, 2),
          NonEmptyList.of(6, 5, 3, 3, 2),
          NonEmptyList.of(3, 3, 5, 4, 9),
          NonEmptyList.of(3, 5, 3, 9, 0)
        )
      ).rotatedCCW,
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of(3, 2, 2, 9, 0),
          NonEmptyList.of(7, 1, 3, 4, 9),
          NonEmptyList.of(3, 5, 3, 5, 3),
          NonEmptyList.of(0, 5, 5, 3, 5),
          NonEmptyList.of(3, 2, 6, 3, 3)
        )
      )
    )
  }

  property("NonEmptyMatrix rotateCW followed by rotateCCW returns same NonEmptyMatrix") {
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(nem => assertEquals(nem, nem.rotatedCW.rotatedCCW))
  }

  property("NonEmptyMatrix rotateCCW followed by rotateCW returns same NonEmptyMatrix") {
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(nem => assertEquals(nem, nem.rotatedCCW.rotatedCW))
  }

  property("NonEmptyMatrix rotateCW 4 times returns same NonEmptyMatrix") {
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(nem => assertEquals(nem, nem.rotatedCW.rotatedCW.rotatedCW.rotatedCW))
  }

  property("NonEmptyMatrix rotateCCW 4 times returns same NonEmptyMatrix") {
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(nem => assertEquals(nem, nem.rotatedCCW.rotatedCCW.rotatedCCW.rotatedCCW))
  }

  test("NonEmptyMatrix map returns correct value") {
    val nem = NonEmptyMatrix(
      NonEmptyList.of(
        NonEmptyList.of(1, 2, 3),
        NonEmptyList.of(4, 5, 6)
      )
    )
    assertEquals(
      nem.map(_ * 3),
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of(3, 6, 9),
          NonEmptyList.of(12, 15, 18)
        )
      )
    )
  }

  test("NonEmptyMatrix tupled returns correct value") {
    val nem1 = NonEmptyMatrix(
      NonEmptyList.of(
        NonEmptyList.of(1, 2, 3),
        NonEmptyList.of(4, 5, 6)
      )
    )
    val nem2 = NonEmptyMatrix(
      NonEmptyList.of(
        NonEmptyList.of('a', 'b', 'c'),
        NonEmptyList.of('d', 'e', 'f')
      )
    )
    assertEquals(
      (nem1, nem2).tupled,
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of((1, 'a'), (2, 'b'), (3, 'c')),
          NonEmptyList.of((4, 'd'), (5, 'e'), (6, 'f'))
        )
      )
    )
  }

  test("NonEmptyMatrix mapN returns correct value") {
    val nem1 = NonEmptyMatrix(
      NonEmptyList.of(
        NonEmptyList.of(1, 2, 3),
        NonEmptyList.of(4, 5, 6)
      )
    )
    val nem2 = NonEmptyMatrix(
      NonEmptyList.of(
        NonEmptyList.of('a', 'b', 'c'),
        NonEmptyList.of('d', 'e', 'f')
      )
    )
    assertEquals(
      (nem1, nem2).mapN { case (i, c) => s"$i-$c" },
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of("1-a", "2-b", "3-c"),
          NonEmptyList.of("4-d", "5-e", "6-f")
        )
      )
    )
  }

  test("getTreeVisibilities(forest) returns correct values") {
    assertEquals(
      getTreeVisibilities(forest),
      NonEmptyMatrix(
        NonEmptyList.of(
          NonEmptyList.of(
            TreeVisibility(left = Visible, top = Visible, right = Hidden, bottom = Hidden),
            TreeVisibility(left = Hidden, top = Visible, right = Hidden, bottom = Hidden),
            TreeVisibility(left = Hidden, top = Visible, right = Hidden, bottom = Hidden),
            TreeVisibility(left = Visible, top = Visible, right = Visible, bottom = Hidden),
            TreeVisibility(left = Hidden, top = Visible, right = Visible, bottom = Hidden)
          ),
          NonEmptyList.of(
            TreeVisibility(left = Visible, top = Hidden, right = Hidden, bottom = Hidden),
            TreeVisibility(left = Visible, top = Visible, right = Hidden, bottom = Hidden),
            TreeVisibility(left = Hidden, top = Visible, right = Visible, bottom = Hidden),
            hiddenFromAllSides,
            TreeVisibility(left = Hidden, top = Hidden, right = Visible, bottom = Hidden)
          ),
          NonEmptyList.of(
            TreeVisibility(left = Visible, top = Visible, right = Visible, bottom = Visible),
            TreeVisibility(left = Hidden, top = Hidden, right = Visible, bottom = Hidden),
            hiddenFromAllSides,
            TreeVisibility(left = Hidden, top = Hidden, right = Visible, bottom = Hidden),
            TreeVisibility(left = Hidden, top = Hidden, right = Visible, bottom = Hidden)
          ),
          NonEmptyList.of(
            TreeVisibility(left = Visible, top = Hidden, right = Hidden, bottom = Hidden),
            hiddenFromAllSides,
            TreeVisibility(left = Visible, top = Hidden, right = Hidden, bottom = Visible),
            hiddenFromAllSides,
            visibleFromAllSides
          ),
          NonEmptyList.of(
            TreeVisibility(left = Visible, top = Hidden, right = Hidden, bottom = Visible),
            TreeVisibility(left = Visible, top = Hidden, right = Hidden, bottom = Visible),
            TreeVisibility(left = Hidden, top = Hidden, right = Hidden, bottom = Visible),
            visibleFromAllSides,
            TreeVisibility(left = Hidden, top = Hidden, right = Visible, bottom = Visible)
          )
        )
      )
    )
  }

  test("getTreesVisibleFromOutsideCount(forest) returns 21") {
    assertEquals(getTreesVisibleFromOutsideCount(forest), 21L)
  }

  test("getTreesVisibleFromOutsideCount(bigInput) returns Some(1796)") {
    assertEquals(getTreesVisibleFromOutsideCount(bigInput), Some(1796L))
  }

}
object Day8Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day8_input.txt")

  def forest: Forest = forestInts.map(i => Tree(Height(i)))

  def forestInts: NonEmptyMatrix[Int] =
    NonEmptyMatrix(
      NonEmptyList.of(
        NonEmptyList.of(3, 0, 3, 7, 3),
        NonEmptyList.of(2, 5, 5, 1, 2),
        NonEmptyList.of(6, 5, 3, 3, 2),
        NonEmptyList.of(3, 3, 5, 4, 9),
        NonEmptyList.of(3, 5, 3, 9, 0)
      )
    )

  def forestGen: Gen[Forest] = nonEmptyMatrixGen(treeGen)
  def forestGen(rows: Int, columns: Int): Gen[Forest] = nonEmptyMatrixGen(rows, columns, treeGen)

  def treeRowGen: Gen[NonEmptyList[Tree]] = Gen.chooseNum(1, 20).flatMap(treeRowGen)
  def treeRowGen(n: Int): Gen[NonEmptyList[Tree]] = nelGen(n, treeGen)

  def treeGen: Gen[Tree] = heightGen.map(Tree.apply)
  def heightGen: Gen[Height] = Gen.chooseNum(0, 9).map(Height.apply)

  def nonEmptyMatrixGen[A](aGen: Gen[A]): Gen[NonEmptyMatrix[A]] =
    Gen.zip(Gen.chooseNum(1, 10), Gen.chooseNum(1, 10)).flatMap { case (r, c) => nonEmptyMatrixGen(r, c, aGen) }
  def nonEmptyMatrixGen[A](rows: Int, columns: Int, aGen: Gen[A]): Gen[NonEmptyMatrix[A]] =
    nelGen(rows, nelGen(columns, aGen)).map(NonEmptyMatrix.apply)
  def nelGen[A](n: Int, aGen: Gen[A]): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(n - 1, aGen)).map { case (a, as) => NonEmptyList(a, as) }

}
