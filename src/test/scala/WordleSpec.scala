import munit.ScalaCheckSuite
import Wordle._
import Wordle.Char._
import Wordle.PositionStatus._

class WordleSpec extends ScalaCheckSuite {

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

  // TODO: fix!!! 🔥🔥🔥
//  test(
//    "getGuessStatus when guess contains twice a char present in the solution only once," +
//      " both in the correct and in an incorrect position"
//  ) {
//    assertEquals(
//      getGuessStatus(Solution[Char](Word(S, P, L, I, T)), Guess[Char](Word(P, I, L, L, S))),
//      GuessStatus[Char](
//        Word(
//          (P, IncorrectPosition),
//          (I, IncorrectPosition),
//          (L, CorrectPosition),
//          (L, Absent),
//          (S, IncorrectPosition)
//        )
//      )
//    )
//  }

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
