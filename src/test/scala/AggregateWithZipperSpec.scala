import AggregateWithZipperSpec._
import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class AggregateWithZipperSpec extends ScalaCheckSuite {

  test("aggregatedList(List.empty) = List.empty") {
    assertEquals(AggregateWithZipper.aggregatedList(List.empty), List.empty)
  }

  test("aggregatedNel(NEL(1,2,2,3,4,4,4,5)) = NEL((1,1),(2,2),(3,1),(4,3),(5,1))") {
    assertEquals(
      AggregateWithZipper.aggregatedNel(NonEmptyList.of(1, 2, 2, 3, 4, 4, 4, 5)),
      NonEmptyList.of((1, 1), (2, 2), (3, 1), (4, 3), (5, 1))
    )
  }

  test("aggregatedNel(NEL(1,2,2,3,2,2,2,4)) = NEL((1,1),(2,2),(3,1),(2,3),(4,1))") {
    assertEquals(
      AggregateWithZipper.aggregatedNel(NonEmptyList.of(1, 2, 2, 3, 2, 2, 2, 4)),
      NonEmptyList.of((1, 1), (2, 2), (3, 1), (2, 3), (4, 1))
    )
  }

  property("aggregatedNel(nel).map(_._2).sum = nel.length") {
    forAll(nelGen) { nel =>
      val res = AggregateWithZipper.aggregatedNel(nel)
      assertEquals(res.map(_._2).toList.sum, nel.length)
    }
  }

  property("0 < |aggregatedNelAux(nel)| <= |nel|") {
    forAll(nelGen) { nel =>
      val as = AggregateWithZipper.aggregatedNelAux(nel).length
      assert(0 < as)
      assert(as <= nel.length)
    }
  }

  property("generateFrom(aggregatedList(as)) = as") {
    forAll(listGen) { as =>
      assertEquals(generateFrom(AggregateWithZipper.aggregatedList(as)), as)
    }
  }

  property("aggregatedList(generateFrom(aggregated)) = aggregated") {
    forAll(aggregatedGen) { aggregated =>
      assertEquals(AggregateWithZipper.aggregatedList(generateFrom(aggregated)), aggregated)
    }
  }

}

object AggregateWithZipperSpec {

  val elemGen: Gen[Int] = Gen.posNum[Int]
  val listGen: Gen[List[Int]] = Gen.listOf(elemGen)
  def listNGen(n: Int): Gen[List[Int]] = Gen.listOfN(n, elemGen)

  val nelGen: Gen[NonEmptyList[Int]] =
    for {
      head <- elemGen
      tail <- listGen
    } yield NonEmptyList(head, tail)

  def nelNGen(n: Int): Gen[NonEmptyList[Int]] =
    for {
      head <- elemGen
      tail <- listNGen(n - 1)
    } yield NonEmptyList(head, tail)

  def generateFrom[A](aggregated: List[(A, Int)]): List[A] =
    aggregated.flatMap { case (a, n) => List.fill(n)(a) }

  val aggregatedGen: Gen[List[(Char, Int)]] =
    for {
      n <- Gen.choose(0, 100)
      as <- Gen.containerOfN[Set, Char](n, Gen.alphaNumChar).map(_.toList)
      ns <- Gen.listOfN(n, Gen.choose(1, 10))
    } yield as.zip(ns)

}
