import MagicSquareForming._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class MagicSquareFormingSpec extends ScalaCheckSuite {

  val posSquareGen: Gen[Square] = for {
    size <- Gen.chooseNum(0, 99)
    rows <- Gen.listOfN(size, Gen.listOfN(size, Gen.posNum[Int])).map(_.toArray.map(_.toArray))
  } yield Square(size, rows)

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

  test("Square replace with no Replacements returns the same Square") {
    val s = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    val rs = List.empty[Replacement]
    val expectedSquare = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
    )
    assertEquals(s.replace(rs), expectedSquare)
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
    val rs = List(Replacement(1, 1, 9))
    val expectedSquare = Square(
      size = 3,
      rows = Array(
        Array(1, 2, 3),
        Array(4, 9, 6),
        Array(7, 8, 9)
      )
    )
    assertEquals(s.replace(rs), expectedSquare)
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
      Replacement(0, 0, 5),
      Replacement(1, 2, 9),
      Replacement(2, 1, 1)
    )
    val expectedSquare = Square(
      size = 3,
      rows = Array(
        Array(5, 2, 3),
        Array(4, 5, 9),
        Array(7, 1, 9)
      )
    )
    assertEquals(s.replace(rs), expectedSquare)
  }

  test("allPossibleAscendingValues(size = 0) is empty") {
    assertEquals(allPossibleAscendingValues(size = 0), List.empty)
  }

  test("allPossibleAscendingValues(size = 3) is correct") {
    assertEquals(allPossibleAscendingValues(size = 3), List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  property("allPossibleAscendingValues returns ascending values") {
    forAll(Gen.posNum[Int]) { size =>
      val vs = allPossibleAscendingValues(size)
      assertEquals(vs, vs.sorted)
    }
  }

  property("diff is commutative") {
    forAll(Gen.posNum[Int], Gen.posNum[Int]) { (a, b) =>
      assertEquals(diff(a, b), diff(b, a))
    }
  }

}
