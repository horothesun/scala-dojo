package wordle

import cats._
import cats.implicits._
import scala.io.Source
import Models._
import Models.Char._
import Models.GuessResult._
import Models.GuesserFilter._
import Models.GuesserSort._

object Wordle {

  def getGuess[A: Order](guesserConfig: GuesserConfig)(suggestions: Set[Word[A]]): Option[Word[A]] = {
    val filteredGroups = guesserConfig.filter match {
      case Unfiltered => List(suggestions)
      case AllDistinctFirst =>
        val (allDistinct, withDuplicates) = suggestions.partition(_.hasAllDistinct)
        List(allDistinct, withDuplicates)
      case WithDuplicatesFirst =>
        val (allDistinct, withDuplicates) = suggestions.partition(_.hasAllDistinct)
        List(withDuplicates, allDistinct)
    }
    val sortedGroups = guesserConfig.sort match {
      case Ascending  => filteredGroups.map(g => g.toList.sorted)
      case Descending => filteredGroups.map(g => g.toList.sorted.reverse)
      case Shuffled   => filteredGroups.map(g => scala.util.Random.shuffle(g.toList))
      case Unsorted   => filteredGroups
    }
    sortedGroups.flatten.headOption
  }

  def getHistory[A: Order](
    dictionary: Dictionary[A],
    guesserConfig: GuesserConfig,
    solution: Solution[A]
  ): GuessHistory[A] = {
    val getNextStatusAndState = getNextGuessStatusAndState(dictionary, guesserConfig, solution).tupled
    val guessStatuses = List.unfold[GuessStatus[A], (GuessHistory[A], Suggester[A], GuessResult)](
      (GuessHistory.empty, Suggester.All(), Unsolved)
    )(getNextStatusAndState)
    GuessHistory(guessStatuses)
  }

  def getNextGuessStatusAndState[A: Order](
    dictionary: Dictionary[A],
    guesserConfig: GuesserConfig,
    solution: Solution[A]
  ): (GuessHistory[A], Suggester[A], GuessResult) => Option[
    (GuessStatus[A], (GuessHistory[A], Suggester[A], GuessResult))
  ] = (history, suggester, latestGuessResult) =>
    latestGuessResult match {
      case Solved => None
      case Unsolved =>
        history.guessStatuses match {
          case gss if gss.length == 6 => None
          case _ =>
            getNextGuessStatus(dictionary, guesserConfig, solution, suggester).map { gs =>
              val newHistory = GuessHistory(history.guessStatuses :+ gs)
              val newSuggester = suggester.and(Suggester.from(gs))
              val newResult = GuessResult.from(gs)
              (gs, (newHistory, newSuggester, newResult))
            }
        }
    }

  def getNextGuessStatus[A: Order](
    dictionary: Dictionary[A],
    guesserConfig: GuesserConfig,
    solution: Solution[A],
    suggester: Suggester[A]
  ): Option[GuessStatus[A]] = {
    val suggestions = suggester.getSuggestions(dictionary)
    suggestions.toList match {
      case Nil => None
      case _   => getGuess[A](guesserConfig)(suggestions).map(guess => GuessStatus.from(solution, guess))
    }
  }

  def fetchDictionary: Dictionary[Char] = {
    val src = Source.fromFile("src/main/scala/wordle/5-chars-english-words.txt")
    val ws = src.getLines.toList
      .map(_.toList.traverse(Char.apply))
      .collect { case Some(List(c1, c2, c3, c4, c5)) => Word(c1, c2, c3, c4, c5) }
      .toSet
    src.close()
    Dictionary(ws)
  }

  // sbt --error "runMain wordle.Wordle"
  def main(args: Array[String]): Unit = {
//    val solution = Solution(Word[Char](C, O, D, E, R))
//    val guess = Word[Char](D, E, C, O, R)
//    val guessStatus = GuessStatus.from(solution, guess)
//    println(guessStatus.show)
//    val guessResult = GuessResult.from(guessStatus)
//    println(guessResult)

//    println(fetchDictionary.words.map(_.show).take(5).mkString("Dictionary:\n  ", ",\n  ", ",\n  ..."))

//    val history_THUMP = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Absent), (M, Incorrect), (E, Absent), (R, Absent)),
//        Word((M, Incorrect), (O, Absent), (U, Correct), (N, Absent), (T, Incorrect)),
//        Word((T, Correct), (H, Correct), (U, Correct), (M, Correct), (B, Absent)),
//        Word((T, Correct), (H, Correct), (U, Correct), (M, Correct), (P, Correct))
//      ).map(GuessStatus.apply)
//    )

//    val history_SCAMP = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Incorrect), (M, Incorrect), (E, Absent), (R, Absent)),
//        Word((A, Incorrect), (M, InKrrect), (P, Incorrect), (L, Absent), (Y, Absent)),
//        Word((S, Correct), (W, Absent), (A, Correct), (M, Correct), (P, Correct)),
//        Word((S, Correct), (C, Correct), (A, Correct), (M, Correct), (P, Correct))
//      ).map(GuessStatus.apply)
//    )

//    val history_CONDO = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Absent), (M, Absent), (E, Absent), (R, Absent)),
//        Word((B, Absent), (O, Correct), (N, Correct), (U, Absent), (S, Absent)),
//        Word((T, Absent), (O, Correct), (N, Correct), (I, Absent), (C, Incorrect)),
//        Word((C, Correct), (O, Correct), (N, Correct), (C, Absent), (H, Absent)),
//        Word((C, Correct), (O, Correct), (N, Correct), (D, Correct), (O, Correct))
//      ).map(GuessStatus.apply)
//    )

//    val history = GuessHistory[Char](
//      List[Word[(Char, PositionStatus)]](
//        Word((G, Absent), (A, Absent), (M, Absent), (E, Absent), (R, Absent)),
//        Word((B, Absent), (O, Correct), (N, Correct), (U, Absent), (S, Absent))
//      ).map(GuessStatus.apply)
//    )
//    val dict = fetchDictionary
//    val suggestions = Suggester.fromHistory(history).getSuggestions(dict)
//    println(
//      suggestions
//        .map(_.toString)
//        .mkString(s"Suggestions (${suggestions.size}/${dict.words.size}):\n  ", "\n  ", "")
//    )
//    val guess = getGuess[Char](GuesserConfig(GuesserFilter.Unfiltered, GuesserSort.Shuffled))(suggestions)
//    println(s"Guess: ${guess.fold(ifEmpty = "<N/D>")(_.toString)}")

    val matchHistory = getHistory[Char](
      dictionary = fetchDictionary,
      GuesserConfig(GuesserFilter.Unfiltered, GuesserSort.Ascending),
      Solution(Word[Char](S, C, A, L, E))
    )
    val guessResult = matchHistory.guessStatuses.lastOption.map(GuessResult.from).getOrElse(GuessResult.Unsolved)
    println(matchHistory.show)
    println(s"Attempts: ${matchHistory.guessStatuses.length} out of 6")
    println(s"Match result: ${guessResult.show}")
  }

}
