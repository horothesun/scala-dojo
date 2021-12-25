import MyList._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Gen.{listOf, negNum, nonEmptyListOf, oneOf, posNum}
import org.scalacheck.Prop.{forAll, propBoolean}

final class MyListSpec extends ScalaCheckSuite {

  val intGen: Gen[Int] = oneOf(posNum[Int], negNum[Int])
  val myListGen: Gen[MyList[Int]] = listOf(intGen).map(MyList.apply(_: _*))
  val nonEmptyMyListGen: Gen[MyList[Int]] = nonEmptyListOf(intGen).map(MyList.apply(_: _*))

  property("tail is defined for a non-empty list") {
    forAll(nonEmptyMyListGen)(tail(_).isDefined)
  }

  test("tail is None for MyNil") {
    assert(tail(MyNil).isEmpty)
  }

  property("given a non-empty list 'as', tail(MyList(as)) is Some(MyList(as.drop(1)))") {
    forAll(nonEmptyListOf(intGen)) { as =>
      tail(MyList(as: _*)).contains(MyList(as.drop(1): _*))
    }
  }

  property("length(MyList(as: _*)) is as.length") {
    forAll(nonEmptyListOf(intGen)) { as =>
      length(MyList(as: _*)) == as.length
    }
  }

  test("setHead(5, MyNil) is None") {
    assertEquals(setHead(5, MyNil), None)
  }

  test("setHead(5, MyList(1, 2, 3)) is Some(MyList(5, 2, 3))") {
    assertEquals(setHead(5, MyList(1, 2, 3)), Some(MyList(5, 2, 3)))
  }

  property("setHead preserves list size") {
    forAll(intGen, nonEmptyListOf(intGen)) { (n, as) =>
      setHead(n, MyList(as: _*)).map(length).contains(as.length)
    }
  }

  property("drop(MyNil, n) is MyNil for any n") {
    forAll(intGen)(n => drop(MyNil, n) == MyNil)
  }

  property("drop(as, n) is 'as' for any negative n") {
    forAll(myListGen, negNum[Int])((as, n) => drop(as, n) == as)
  }

  property("MyList drop is same as List's") {
    forAll(listOf(intGen), intGen)((as, n) => drop(MyList(as: _*), n) == MyList(as.drop(n): _*))
  }

  property("MyList dropWhile is same as List's") {
    forAll(listOf(intGen)) { as =>
      def isNonNegative(i: Int): Boolean = i >= 0
      dropWhile(MyList(as: _*), isNonNegative) == MyList(as.dropWhile(isNonNegative): _*)
    }
  }

  property("MyList 'init' is same as List's dropRight(1)") {
    forAll(listOf(intGen))(as => init(MyList(as: _*)) == MyList(as.dropRight(1): _*))
  }

}
