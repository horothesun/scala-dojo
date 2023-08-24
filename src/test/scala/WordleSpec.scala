import cats.Order
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import Wordle._
import Wordle.Char._
import Wordle.PositionStatus._
import WordleSpec._
import WordleSpec.TestChar._

class WordleSpec extends ScalaCheckSuite {

  property("word.contains(char) ==> word.occurrences(char).isDefined") {
    forAll(wordGen, testCharGen) { (w, c) =>
      w.contains(c) ==> w.occurrences(Order[TestChar])(c).isDefined
    }
  }

  test("getGuessStatus when guess matches solution on all positions") {
    assertEquals(
      getGuessStatus(Solution[Char](Word(G, A, M, E, R)), Guess[Char](Word(G, A, M, E, R))),
      GuessStatus[Char](
        Word(
          (G, CorrectPosition),
          (A, CorrectPosition),
          (M, CorrectPosition),
          (E, CorrectPosition),
          (R, CorrectPosition)
        )
      )
    )
  }

  test("getGuessStatus when guess is solution's anagram and matches one position") {
    assertEquals(
      getGuessStatus(Solution[Char](Word(C, O, D, E, R)), Guess[Char](Word(D, E, C, O, R))),
      GuessStatus[Char](
        Word(
          (D, IncorrectPosition),
          (E, IncorrectPosition),
          (C, IncorrectPosition),
          (O, IncorrectPosition),
          (R, CorrectPosition)
        )
      )
    )
  }

  test(
    "getGuessStatus when guess contains twice a char present in the solution only once," +
      " both in the correct and in an incorrect position"
  ) {
    assertEquals(
      getGuessStatus(Solution[Char](Word(S, P, L, I, T)), Guess[Char](Word(P, I, L, L, S))),
      GuessStatus[Char](
        Word(
          (P, IncorrectPosition),
          (I, IncorrectPosition),
          (L, CorrectPosition),
          (L, Absent),
          (S, IncorrectPosition)
        )
      )
    )
  }

  test(
    "getGuessStatus when guess contains twice a char present in the solution twice," +
      " both in the correct and in an incorrect position"
  ) {
    assertEquals(
      getGuessStatus(Solution[Char](Word(S, P, I, L, L)), Guess[Char](Word(P, I, L, L, S))),
      GuessStatus[Char](
        Word(
          (P, IncorrectPosition),
          (I, IncorrectPosition),
          (L, IncorrectPosition),
          (L, CorrectPosition),
          (S, IncorrectPosition)
        )
      )
    )
  }

  test(
    "getGuessStatus when guess contains twice a char present in the solution twice, " +
      "both in the incorrect position"
  ) {
    assertEquals(
      getGuessStatus(Solution[Char](Word(S, P, I, L, L)), Guess[Char](Word(L, L, O, Y, D))),
      GuessStatus[Char](
        Word(
          (L, IncorrectPosition),
          (L, IncorrectPosition),
          (O, Absent),
          (Y, Absent),
          (D, Absent)
        )
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
