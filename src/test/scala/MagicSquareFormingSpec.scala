import MagicSquareForming._
import MagicSquareFormingSpec._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, propBoolean}

class MagicSquareFormingSpec extends ScalaCheckSuite {

  test("Square columns is correct") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    assertEquals(
      s.columns.toList.map(_.toList),
      Array(
        Array(1, 4, 7),
        Array(2, 5, 8),
        Array(3, 6, 9)
      ).toList.map(_.toList)
    )
  }

  property("Square columns has same rows sizes") {
    forAll(posSquareGen) { s =>
      val columnSizes = s.columns.map(c => c.length).toList
      assert(columnSizes.forall(_ == s.size))
      assertEquals(
        s.rows.map(r => r.length).toList,
        columnSizes
      )
    }
  }

  test("Square topLeftBottomRightDiagonal is correct") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    assertEquals(
      s.topLeftBottomRightDiagonal.toList,
      List(1, 5, 9)
    )
  }

  property("Square topLeftBottomRightDiagonal has same square size") {
    forAll(posSquareGen) { s =>
      assertEquals(s.size, s.topLeftBottomRightDiagonal.length)
    }
  }

  test("Square topRightBottomLeftDiagonal is correct") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    assertEquals(
      s.topRightBottomLeftDiagonal.toList,
      List(3, 5, 7)
    )
  }

  property("Square topRightBottomLeftDiagonal has same square size") {
    forAll(posSquareGen) { s =>
      assertEquals(s.size, s.topRightBottomLeftDiagonal.length)
    }
  }

  test("Square isMagic is correctly true") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(8, 3, 4),
        Array(1, 5, 9),
        Array(6, 7, 2)
      )
    )
    assert(s.isMagic)
  }

  test("Square isMagic is correctly false") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    assert(!s.isMagic)
  }

  test("Square magicConstant is None for non-magic Square") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    assertEquals(s.magicConstant, None)
  }

  test("Square magicConstant is Some(...) for magic Square") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(8, 3, 4),
        Array(1, 5, 9),
        Array(6, 7, 2)
      )
    )
    assertEquals(s.magicConstant, Some(15))
  }

  test("replace with no Replacements returns the same Square") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    val rs = List.empty[Replacement]
    val expectedResult = ReplaceResult(
      Cost(0),
      Square(
        size = 3,
        rows = Array(
          Array(1, 2, 3),
          Array(4, 5, 6),
          Array(7, 8, 9)
        )
      )
    )
    assertEquals(replace(s, rs), expectedResult)
  }

  test("Square replace with single Replacement is correct") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    val rs = List(Replacement(newValue = 9, Position(rowIndex = 1, columnIndex = 1)))
    val expectedResult = ReplaceResult(
      Cost(4),
      Square(
        size = 3,
        rows = Array(
          Array(1, 2, 3),
          Array(4, 9, 6),
          Array(7, 8, 9)
        )
      )
    )
    assertEquals(replace(s, rs), expectedResult)
  }

  test("Square replace with 3 Replacements is correct") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    val rs = List(
      Replacement(newValue = 5, Position(rowIndex = 0, columnIndex = 0)),
      Replacement(newValue = 9, Position(rowIndex = 1, columnIndex = 2)),
      Replacement(newValue = 1, Position(rowIndex = 2, columnIndex = 1))
    )
    val expectedResult = ReplaceResult(
      Cost(4 + 3 + 7),
      Square(
        size = 3,
        rows = Array(
          Array(5, 2, 3),
          Array(4, 5, 9),
          Array(7, 1, 9)
        )
      )
    )
    assertEquals(replace(s, rs), expectedResult)
  }

  property("cost is commutative") {
    forAll(Gen.posNum[Int], Gen.posNum[Int]) { (a, b) =>
      assertEquals(cost(a, b), cost(b, a))
    }
  }

  test("allValues(size = 0) is empty") {
    assertEquals(allValues(size = 0), List.empty)
  }

  test("allValues(size = 3) is correct") {
    assertEquals(allValues(size = 3), List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  property("allValues returns ascending values") {
    forAll(Gen.posNum[Int]) { size =>
      val vs = allValues(size)
      assertEquals(vs, vs.sorted)
    }
  }

  test("allPositions(size = 3) is correct") {
    assertEquals(
      allPositions(size = 3),
      List(
        Position(rowIndex = 0, columnIndex = 0),
        Position(rowIndex = 0, columnIndex = 1),
        Position(rowIndex = 0, columnIndex = 2),
        Position(rowIndex = 1, columnIndex = 0),
        Position(rowIndex = 1, columnIndex = 1),
        Position(rowIndex = 1, columnIndex = 2),
        Position(rowIndex = 2, columnIndex = 0),
        Position(rowIndex = 2, columnIndex = 1),
        Position(rowIndex = 2, columnIndex = 2)
      )
    )
  }

  property("allPositions(size) returns a List of size^2 length") {
    forAll(Gen.posNum[Int]) { size =>
      assertEquals(allPositions(size).size, size * size)
    }
  }

  test("allReplacements(size = 2) is correct") {
    assertEquals(
      allReplacements(size = 2),
      List(
        Replacement(newValue = 1, Position(rowIndex = 0, columnIndex = 0)),
        Replacement(newValue = 2, Position(rowIndex = 0, columnIndex = 0)),
        Replacement(newValue = 3, Position(rowIndex = 0, columnIndex = 0)),
        Replacement(newValue = 4, Position(rowIndex = 0, columnIndex = 0)),
        Replacement(newValue = 1, Position(rowIndex = 0, columnIndex = 1)),
        Replacement(newValue = 2, Position(rowIndex = 0, columnIndex = 1)),
        Replacement(newValue = 3, Position(rowIndex = 0, columnIndex = 1)),
        Replacement(newValue = 4, Position(rowIndex = 0, columnIndex = 1)),
        Replacement(newValue = 1, Position(rowIndex = 1, columnIndex = 0)),
        Replacement(newValue = 2, Position(rowIndex = 1, columnIndex = 0)),
        Replacement(newValue = 3, Position(rowIndex = 1, columnIndex = 0)),
        Replacement(newValue = 4, Position(rowIndex = 1, columnIndex = 0)),
        Replacement(newValue = 1, Position(rowIndex = 1, columnIndex = 1)),
        Replacement(newValue = 2, Position(rowIndex = 1, columnIndex = 1)),
        Replacement(newValue = 3, Position(rowIndex = 1, columnIndex = 1)),
        Replacement(newValue = 4, Position(rowIndex = 1, columnIndex = 1))
      )
    )
  }

  property("allReplacements(size) returns a List of size^4 length") {
    forAll(Gen.chooseNum(0, 10)) { size =>
      assertEquals(allReplacements(size).size, size * size * size * size)
    }
  }

  property("if r.isNoOpFor(s) then r preserves s") {
    forAll(replacementAndNonEmptySquare) { case (r, s) =>
      r.isNoOpFor(s) ==> (replace(s, List(r)).square == s)
    }
  }

  property("if r preserves s then r.isNoOpFor(s)") {
    forAll(replacementAndNonEmptySquare) { case (r, s) =>
      (replace(s, List(r)).square == s) ==> r.isNoOpFor(s)
    }
  }

  test("minCostReplacements is correct on sample 0") {
    val square0 = Square(
      size = 3,
      rows = Array(
        Array(4, 9, 2),
        Array(3, 5, 7),
        Array(8, 1, 5)
      )
    )
    assertEquals(
      minCostReplacements(square0),
      Candidate(
        replacements = List(Replacement(newValue = 6, Position(rowIndex = 2, columnIndex = 2))),
        cost = Cost(1),
        square = Square(
          size = 3,
          rows = Array(
            Array(4, 9, 2),
            Array(3, 5, 7),
            Array(8, 1, 6)
          )
        )
      )
    )
  }

  test("minCostReplacements is correct on sample 1") {
    val square1 = Square(
      size = 3,
      rows = Array(
        Array(4, 8, 2),
        Array(4, 5, 7),
        Array(6, 1, 6)
      )
    )
    assertEquals(
      minCostReplacements(square1),
      Candidate(
        replacements = List(
          Replacement(newValue = 9, Position(rowIndex = 0, columnIndex = 1)),
          Replacement(newValue = 3, Position(rowIndex = 1, columnIndex = 0)),
          Replacement(newValue = 8, Position(rowIndex = 2, columnIndex = 0))
        ),
        cost = Cost(4),
        square = Square(
          size = 3,
          rows = Array(
            Array(4, 9, 2),
            Array(3, 5, 7),
            Array(8, 1, 6)
          )
        )
      )
    )
  }

  test("minCostReplacements(square0) only replaces distinct positions") {
    val square0 = Square(
      size = 3,
      rows = Array(
        Array(4, 9, 2),
        Array(3, 5, 7),
        Array(8, 1, 5)
      )
    )
    val replacedPositions = minCostReplacements(square0).replacements.map(_.position)
    assertEquals(replacedPositions, replacedPositions.distinct)
  }

  test("minCostReplacements(square1) only replaces distinct positions") {
    val square1 = Square(
      size = 3,
      rows = Array(
        Array(4, 8, 2),
        Array(4, 5, 7),
        Array(6, 1, 6)
      )
    )
    val replacedPositions = minCostReplacements(square1).replacements.map(_.position)
    assertEquals(replacedPositions, replacedPositions.distinct)
  }

  test("formingMagicSquare is correct on sample 0") {
    val sample0 = Array(
      Array(4, 9, 2),
      Array(3, 5, 7),
      Array(8, 1, 5)
    )
    assertEquals(formingMagicSquare(sample0), 1)
  }

  test("formingMagicSquare is correct on sample 1") {
    val sample1 = Array(
      Array(4, 8, 2),
      Array(4, 5, 7),
      Array(6, 1, 6)
    )
    assertEquals(formingMagicSquare(sample1), 4)
  }

}

object MagicSquareFormingSpec {

  val posSquareGen: Gen[Square] =
    Gen
      .chooseNum(0, 99)
      .flatMap(size => squareGen(size, elementGen = Gen.posNum[Int]))

  def squareGen(size: Int, elementGen: Gen[Int]): Gen[Square] =
    Gen
      .listOfN(size, Gen.listOfN(size, elementGen))
      .map(_.toArray.map(_.toArray))
      .map(rows => Square(size, rows))

  def positionGen(nonNegativeSize: Int): Gen[Position] = for {
    rowIndex <- Gen.chooseNum(0, nonNegativeSize - 1)
    columnIndex <- Gen.chooseNum(0, nonNegativeSize - 1)
  } yield Position(rowIndex, columnIndex)

  def replacementGen(nonNegativeSize: Int): Gen[Replacement] = for {
    newValue <- Gen.chooseNum(1, nonNegativeSize * nonNegativeSize)
    position <- positionGen(nonNegativeSize)
  } yield Replacement(newValue, position)

  def replacementAndNonEmptySquare: Gen[(Replacement, Square)] = for {
    nonNegativeSize <- Gen.chooseNum(1, 25)
    r <- replacementGen(nonNegativeSize)
    s <- squareGen(nonNegativeSize, elementGen = Gen.chooseNum(1, 10))
  } yield (r, s)

}
