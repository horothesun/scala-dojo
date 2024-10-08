package wordle

import Models._
import Models.Char._
import Models.GuesserFilter._
import Models.GuesserSort._
import Models.GuessResult._
import Models.PositionStatus._
import Models.Word._
import cats._
import cats.syntax.all._
import scala.io.Source

object Wordle {

  def getMatchHistory[A: Order](
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
    implicit val orderingA: Ordering[A] = Order[A].toOrdering
    val sortedGroups = guesserConfig.sort match {
      case Ascending  => filteredGroups.map(g => g.toList.sorted)
      case Descending => filteredGroups.map(g => g.toList.sorted.reverse)
      case Shuffled   => filteredGroups.map(g => scala.util.Random.shuffle(g.toList))
      case Unsorted   => filteredGroups
    }
    sortedGroups.flatten.headOption
  }

  def fetchDictionary: Dictionary[Char] = {
    val src = Source.fromFile("src/main/scala/wordle/5-chars-english-words.txt")
    val ws =
      src
        .getLines()
        .toList
        .map(_.toList.traverse(Char.apply))
        .collect { case Some(List(c1, c2, c3, c4, c5)) => Word(c1, c2, c3, c4, c5) }
        .toSet
    src.close
    Dictionary(ws)
  }

  def assistMe(guesserConfig: GuesserConfig, history: GuessHistory[Char]): Unit = {
    val dict = fetchDictionary
    val suggestions = Suggester.fromHistory(history).getSuggestions(dict)
    println(
      suggestions
        .map(_.toString)
        .mkString(s"Suggestions (${suggestions.size}/${dict.words.size}):\n  ", "\n  ", "")
    )
    val guess = getGuess[Char](guesserConfig)(suggestions)
    println(s"Guess: ${guess.fold(ifEmpty = "<N/D>")(_.toString)}")
  }

  def playMatch(guesserConfig: GuesserConfig, solution: Solution[Char]): Unit = {
    val matchHistory = getMatchHistory[Char](dictionary = fetchDictionary, guesserConfig, solution)
    val guessResult = matchHistory.guessStatuses.lastOption.map(GuessResult.from).getOrElse(GuessResult.Unsolved)
    println(matchHistory.show)
    println(s"Attempts: ${matchHistory.guessStatuses.length} out of 6")
    println(s"Match result: ${guessResult.show}")
  }

  def main(args: Array[String]): Unit = {
    playMatch(
      GuesserConfig(GuesserFilter.Unfiltered, GuesserSort.Ascending),
      Solution(Word[Char](S, C, A, L, E))
    )
    println("")
//    assistMe(
//      GuesserConfig(GuesserFilter.Unfiltered, GuesserSort.Shuffled),
//      GuessHistory(
//        List[Word[(Char, PositionStatus)]](
//          Word((G, Absent), (U, Absent), (E, Absent), (S, Absent), (S, Absent)),
//          Word((B, Absent), (L, Correct), (O, Absent), (C, Correct), (K, Correct))
//        ).map(GuessStatus.apply)
//      )
//    )
  }

}
