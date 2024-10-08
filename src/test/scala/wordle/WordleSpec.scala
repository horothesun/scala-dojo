package wordle

import Models._
import Models.Char._
import Models.PositionStatus._
import Models.WordPos._
import Suggester._
import WordleSpec._
import WordleSpec.TestChar._
import cats.Order
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class WordleSpec extends ScalaCheckSuite {

  property("word.contains(char) ==> word.occurrences(char).isDefined") {
    forAll(wordGen, testCharGen) { (w, c) =>
      w.contains(c) ==> w.occurrences(Order[TestChar])(c).isDefined
    }
  }

  test("GuessStatus.from(...) when guess doesn't have any of solution's chars") {
    assertEquals(
      GuessStatus.from(Solution(Word[Char](T, H, I, N, K)), Word[Char](G, A, M, E, R)),
      GuessStatus[Char](Word((G, Absent), (A, Absent), (M, Absent), (E, Absent), (R, Absent)))
    )
  }

  test("GuessStatus.from(...) when guess matches solution on all positions") {
    assertEquals(
      GuessStatus.from(Solution(Word[Char](G, A, M, E, R)), Word[Char](G, A, M, E, R)),
      GuessStatus[Char](Word((G, Correct), (A, Correct), (M, Correct), (E, Correct), (R, Correct)))
    )
  }

  test("GuessStatus.from(...) when guess is solution's anagram and matches one position") {
    assertEquals(
      GuessStatus.from(Solution(Word[Char](C, O, D, E, R)), Word[Char](D, E, C, O, R)),
      GuessStatus[Char](Word((D, Incorrect), (E, Incorrect), (C, Incorrect), (O, Incorrect), (R, Correct)))
    )
  }

  test(
    "GuessStatus.from(...) when guess contains twice a char present in the solution only once," +
      " both in the correct and in an incorrect position"
  ) {
    assertEquals(
      GuessStatus.from(Solution(Word[Char](S, P, L, I, T)), Word[Char](P, I, L, L, S)),
      GuessStatus[Char](Word((P, Incorrect), (I, Incorrect), (L, Correct), (L, Absent), (S, Incorrect)))
    )
  }

  test(
    "GuessStatus.from(...) when guess contains twice a char present in the solution twice," +
      " both in the correct and in an incorrect position"
  ) {
    assertEquals(
      GuessStatus.from(Solution(Word[Char](S, P, I, L, L)), Word[Char](P, I, L, L, S)),
      GuessStatus[Char](Word((P, Incorrect), (I, Incorrect), (L, Incorrect), (L, Correct), (S, Incorrect)))
    )
  }

  test(
    "GuessStatus.from(...) when guess contains twice a char present in the solution twice, " +
      "both in the incorrect position"
  ) {
    assertEquals(
      GuessStatus.from(Solution(Word[Char](S, P, I, L, L)), Word[Char](L, L, O, Y, D)),
      GuessStatus[Char](Word((L, Incorrect), (L, Incorrect), (O, Absent), (Y, Absent), (D, Absent)))
    )
  }

  test("Suggester from all absent and unique chars GuessStatus") {
    assertEquals(
      Suggester.from[TestChar](
        GuessStatus(Word((C1, Absent), (C2, Absent), (C3, Absent), (C4, Absent), (C5, Absent)))
      ),
      And[TestChar](
        And(
          And(
            And(
              AbsentFromWord(C1),
              AbsentFromWord(C2)
            ),
            AbsentFromWord(C3)
          ),
          AbsentFromWord(C4)
        ),
        AbsentFromWord(C5)
      )
    )
  }

  test("Suggester from 2 absent duplicate chars and 3 unique correct GuessStatus") {
    assertEquals(
      Suggester.from[TestChar](
        GuessStatus(Word((C1, Absent), (C1, Absent), (C2, Correct), (C3, Correct), (C4, Correct)))
      ),
      And[TestChar](
        And(
          And(
            And(
              AbsentFromWord(C1),
              AbsentFromWord(C1)
            ),
            MatchingPosition(C2, Pos3)
          ),
          MatchingPosition(C3, Pos4)
        ),
        MatchingPosition(C4, Pos5)
      )
    )
  }

}

object WordleSpec {

  sealed trait TestChar
  object TestChar {
    case object C1 extends TestChar
    case object C2 extends TestChar
    case object C3 extends TestChar
    case object C4 extends TestChar
    case object C5 extends TestChar
    case object C6 extends TestChar

    implicit val ordering: Ordering[TestChar] = Ordering[String].on[TestChar](_.toString)
    implicit val order: Order[TestChar] = Order.fromOrdering[TestChar]
  }

  val testCharGen: Gen[TestChar] = Gen.oneOf(C1, C2, C3, C4, C5, C6)
  val wordGen: Gen[Word[TestChar]] =
    Gen
      .zip(testCharGen, testCharGen, testCharGen, testCharGen, testCharGen)
      .map((Word.apply[TestChar] _).tupled)

}
