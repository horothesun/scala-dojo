import munit.ScalaCheckSuite
import Wordle._
import Wordle.Char._
import Wordle.PositionStatus._

class WordleSpec extends ScalaCheckSuite {

  test("getGuessStatus(solution = CODER, guess = DECOR) = DECO(wrong-position) R(ok)") {
    assertEquals(
      getGuessStatus(Solution[Char](Word(C, O, D, E, R)), Guess[Char](Word(D, E, C, O, R))),
      GuessStatus[Char](
        Word(
          (D, PresentButIncorrectPosition),
          (E, PresentButIncorrectPosition),
          (C, PresentButIncorrectPosition),
          (O, PresentButIncorrectPosition),
          (R, PresentAndCorrectPosition)
        )
      )
    )
  }

//  test("getGuessStatus(solution = SPLIT, guess = PILLS) = PI(wrong-position) L(ok) L(ko) S(wrong-position)") {
//    assertEquals(
//      getGuessStatus(Solution[Char](Word(S, P, L, I, T)), Guess[Char](Word(P, I, L, L, S))),
//      GuessStatus[Char](
//        Word(
//          (P, PresentButIncorrectPosition),
//          (I, PresentButIncorrectPosition),
//          (L, PresentAndCorrectPosition),
//          (L, NotPresent),
//          (S, PresentButIncorrectPosition)
//        )
//      )
//    )
//  }

}
