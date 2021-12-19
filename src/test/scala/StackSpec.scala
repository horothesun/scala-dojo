import Stack._
import org.scalacheck.Gen.{listOf, negNum, oneOf, posNum}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}

object StackSpec extends Properties("Stack") {

  val elementGen: Gen[Int]           = oneOf(posNum[Int], negNum[Int])
  val elementListGen: Gen[List[Int]] = listOf(elementGen)

  val stackGen: Gen[Stack[Int]] = elementListGen.map(stackFromList)

  property("n pushes followed by n pops leave any Stack unchanged") = forAll(elementListGen, stackGen) { (ts, stack) =>
    val stackAfterPushes        = ts.foldLeft(stack)((s, t) => push(t)(s))
    val stackAfterPushesAndPops = ts.foldLeft(stackAfterPushes)((s, _) => pop(s).map(_._2).getOrElse(EmptyStack))
    stack == stackAfterPushesAndPops
  }

  property("pushing all elements of a list in a stack then popping them out produces the same list") =
    forAll(elementListGen, stackGen) { (ts, stack) =>
      val stackAfterPushes = ts.foldLeft(stack)((s, t) => push(t)(s))
      val poppedList       = ts
        .foldLeft((List[Int](), stackAfterPushes))((res, _) =>
          pop(res._2) match {
            case Some((top, tail)) => (top :: res._1, tail)
            case None              => (res._1, EmptyStack)
          }
        )
        ._1
      ts == poppedList
    }

  property("list and stack with the same elements have the same min") = forAll(elementListGen) { ts =>
    val s = stackFromList(ts)
    min(s) == ts.minOption
  }

  property("safeMin and min return the same result") = forAll(stackGen)(s => safeMin(s) == min(s))

  property("safeMin and foldMin return the same result") = forAll(stackGen)(s => safeMin(s) == foldMin(s))

  property("fromList and toList should produce same list") = forAll { s: List[Int] => stackFromList(s).toList == s }

  property("pushBatch of one element is the same as push") = forAll(elementGen, stackGen) { (t, stack) =>
    pushBatch(List(t))(stack) == push(t)(stack)
  }

  property("popBatch once is the same as pop") = forAll(elementGen, stackGen) { (t, stack) =>
    pushBatch(List(t))(stack) == push(t)(stack)
  }

  property("pushBatch and popBatch in sequence return the same stack and the same list in reverse order ") =
    forAll(elementListGen, stackGen) { (ts, stack) =>
      val r = popBatch(ts.length)(pushBatch(ts)(stack))
      r._2 == stack && r._1 == ts.reverse
    }

  property("two sequential pushBatch are the same as pushBatch of the concatenation of two lists") =
    forAll(elementListGen, elementListGen, stackGen) { (ts1, ts2, stack) =>
      val s1 = pushBatch(ts1)(stack)
      val s2 = pushBatch(ts2)(s1)
      s2 == pushBatch(ts1 ++ ts2)(stack)
    }

  property("popBatch a number of times equal to the size o the stack should always return empty stack") =
    forAll(stackGen) { stack =>
      val n = size(stack)
      val r = popBatch(n)(stack)
      r._2 == EmptyStack && r._1.length == n
    }

  def stackFromList[T](ts: List[T]): Stack[T] =
    ts.foldLeft[Stack[T]](EmptyStack)((s, t) => NonEmptyStack(t, s))
}
