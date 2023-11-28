package adventofcode22

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import Day10._
import Day10.Pixel._
import Day10Suite._

class Day10Suite extends ScalaCheckSuite {

  test("getSignalStrengthAtCyclesOfInterestSum(bigInput) is Some(SignalStrength(17_020))") {
    assertEquals(getSignalStrengthAtCyclesOfInterestSum(bigInput), Some(SignalStrength(17_020)))
  }

  property("getSpriteRow(spriteMidX0Based = x) == allOffRow when x < -1") {
    forAll(Gen.chooseNum(-100, -2)) { x =>
      assertEquals(getSpriteRow(x).toSeq, allOffRow.toSeq)
    }
  }

  test("getSpriteRow(spriteMidX0Based = -1) returns correct value") {
    assertEquals(
      getSpriteRow(spriteMidX0Based = -1).toSeq,
      (Array[Pixel](On) ++ Array.fill[Pixel](39)(Off)).toSeq
    )
  }

  test("getSpriteRow(spriteMidX0Based = 0) returns correct value") {
    assertEquals(
      getSpriteRow(spriteMidX0Based = 0).toSeq,
      (Array[Pixel](On, On) ++ Array.fill[Pixel](38)(Off)).toSeq
    )
  }

  test("getSpriteRow(spriteMidX0Based = 1) returns correct value") {
    assertEquals(
      getSpriteRow(spriteMidX0Based = 1).toSeq,
      (Array[Pixel](On, On, On) ++ Array.fill[Pixel](37)(Off)).toSeq
    )
  }

  property("getSpriteRow(spriteMidX0Based = x) contains a full 3 pixels sprite when 1 <= x <= 38") {
    forAll(Gen.chooseNum(1, 38)) { x =>
      assert(getSpriteRow(x).containsSlice(Array[Pixel](On, On, On)))
    }
  }

  test("getSpriteRow(spriteMidX0Based = 38) returns correct value") {
    assertEquals(
      getSpriteRow(spriteMidX0Based = 38).toSeq,
      (Array.fill[Pixel](37)(Off) ++ Array[Pixel](On, On, On)).toSeq
    )
  }

  test("getSpriteRow(spriteMidX0Based = 39) returns correct value") {
    assertEquals(
      getSpriteRow(spriteMidX0Based = 39).toSeq,
      (Array.fill[Pixel](38)(Off) ++ Array[Pixel](On, On)).toSeq
    )
  }

  test("getSpriteRow(spriteMidX0Based = 40) returns correct value") {
    assertEquals(
      getSpriteRow(spriteMidX0Based = 40).toSeq,
      (Array.fill[Pixel](39)(Off) ++ Array[Pixel](On)).toSeq
    )
  }

  property("getSpriteRow(spriteMidX0Based = x) == allOffRow when x > 40") {
    forAll(Gen.chooseNum(41, 100)) { x =>
      assertEquals(getSpriteRow(x).toSeq, allOffRow.toSeq)
    }
  }

}
object Day10Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day10_input.txt")

}
