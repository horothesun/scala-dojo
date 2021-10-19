import Stack.{pop, push}
import org.scalacheck.Gen.{const, listOf, listOfN, lzy, negNum, oneOf, posNum}
import org.scalacheck.Prop.{collect, forAll, propBoolean}
import org.scalacheck.{Gen, Properties}

object StackSpec extends Properties("Stack") {

  val elementGen: Gen[Int] = oneOf(posNum[Int], negNum[Int])
  val elementListGen: Gen[List[Int]] = posNum[Int].flatMap(listOfN(_, elementGen))
  val emptyStackGen: Gen[EmptyStack[Int]] = const(EmptyStack())
//  val nonEmptyStackGen: Gen[NonEmptyStack[Int]] =
//    for { t <- elementGen; s <- stackGen } yield NonEmptyStack(t, s)
  val nonEmptyStackGen: Gen[NonEmptyStack[Int]] =
    elementGen.flatMap(e => stackGen.map(NonEmptyStack(e, _)))
//  val stackGen: Gen[Stack[Int]] = lzy(oneOf(emptyStackGen, nonEmptyStackGen))
  def stackFromList[T](ts: List[T]): Stack[T] =
    ts.foldLeft(EmptyStack(): Stack[T])((s, t) => NonEmptyStack(t, s))
  val stackGen: Gen[Stack[Int]] = listOf(elementGen).map(stackFromList)

  property("n pushes followed by n pops leave the Stack unchanged") =
    forAll (elementListGen, stackGen) { (ts, stack) =>
      val stackAfterPushes = ts.foldLeft(stack)((s, t) => push(t)(s))
      val stackAfterPushesAndPops = ts.indices.foldLeft(stackAfterPushes)((s, _) =>
        pop(s).map(_._2).getOrElse(EmptyStack())
      )
      stack == stackAfterPushesAndPops
    }

  property("pushing all elements of a list in a stack then popping them out produces the same list") =
    forAll (elementListGen, stackGen) { (ts, stack) =>
      val stackAfterPushes = ts.foldLeft(stack)((s, t) => push(t)(s))
      val poppedList = ts.indices.foldLeft((List[Int](), stackAfterPushes))((res, _) =>
        pop(res._2) match {
          case Some((top, tail)) => (top :: res._1, tail)
          case None => (res._1, EmptyStack())
        })._1
      ts == poppedList
    }
}
