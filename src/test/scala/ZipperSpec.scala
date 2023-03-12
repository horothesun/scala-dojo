import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import ZipperSpec._

class ZipperSpec extends ScalaCheckSuite {

  property("fromNel(nel).toNel = nel") {
    forAll(nelGen) { nel =>
      assertEquals(Zipper.fromNel(nel).toNel, nel)
    }
  }

  property("fromNel(nel).toNelRight = nel") {
    forAll(nelGen) { nel =>
      assertEquals(Zipper.fromNel(nel).toNelRight, nel)
    }
  }

  property("fromNel(NEL(n1,n2,n3)).moveRight.toNelLeft = NEL(n1,n2)") {
    forAll(elemGen, elemGen, elemGen) { (n1, n2, n3) =>
      assertEquals(
        Zipper.fromNel(NonEmptyList.of(n1, n2, n3)).moveRight.toNelLeft,
        NonEmptyList.of(n1, n2)
      )
    }
  }

  property("fromNel(NEL(n1,n2,n3)).moveRight.moveRight.toNelLeft = NEL(n1,n2,n3)") {
    forAll(elemGen, elemGen, elemGen) { (n1, n2, n3) =>
      val nel = NonEmptyList.of(n1, n2, n3)
      assertEquals(Zipper.fromNel(nel).moveRight.moveRight.toNelLeft, nel)
    }
  }

  property("fromNel(nel).moveRight.moveLeft.toNelRight = nel, with |nel| = 10") {
    forAll(nelNGen(10)) { nel =>
      assertEquals(Zipper.fromNel(nel).moveRight.moveLeft.toNelRight, nel)
    }
  }

  property("fromNel(nel).moveRight.moveRight.moveLeft.moveLeft.toNelRight = nel, with |nel| = 10") {
    forAll(nelNGen(10)) { nel =>
      assertEquals(Zipper.fromNel(nel).moveRight.moveRight.moveLeft.moveLeft.toNelRight, nel)
    }
  }

  test("fromNel(NEL(n1,n2,n3)).coflatten") {
    forAll(elemGen, elemGen, elemGen) { (n1, n2, n3) =>
      val nel = NonEmptyList.of(n1, n2, n3)
      assertEquals(
        Zipper.fromNel(nel).coflatten.toNel.map(_.toNelRight),
        NonEmptyList.of(
          NonEmptyList.of(n1, n2, n3),
          NonEmptyList.of(n2, n3),
          NonEmptyList.of(n3)
        )
      )
    }
  }

}

object ZipperSpec {

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

}
