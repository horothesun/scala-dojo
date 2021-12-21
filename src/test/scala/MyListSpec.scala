import MyList._
import org.scalacheck.Gen.{listOf, negNum, nonEmptyListOf, oneOf, posNum}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}

object MyListSpec extends Properties("MyList") {

  val intGen: Gen[Int] = oneOf(posNum[Int], negNum[Int])
  val myListGen: Gen[MyList[Int]] = listOf(intGen).map(MyList.apply(_: _*))
  val nonEmptyMyListGen: Gen[MyList[Int]] = nonEmptyListOf(intGen).map(MyList.apply(_: _*))

  property("tail is defined for a non-empty list") = forAll(nonEmptyMyListGen)(tail(_).isDefined)

  property("given a non-empty list 'as', tail(MyList(as)) is Some(MyList(as.drop(1)))") =
    forAll(nonEmptyListOf(intGen)) { as =>
      tail(MyList(as: _*)).contains(MyList(as.drop(1): _*))
    }

}
