import TextStatistics.{longestWordInSentence, sentencesFromString, wordCount}
import org.scalatest.funsuite.AnyFunSuite

final class TextStatisticsTest extends AnyFunSuite {

  test("produce empty list of Sentences from empty String") {
    assert(sentencesFromString("") == List[Sentence]())
  }

  test("produce singleton list of Sentences from single sentence String") {
    assert(sentencesFromString("lorem ipsum") == List[Sentence](Sentence(List(Word("lorem"), Word("ipsum")))))
  }

  test("produce list of 2 Sentences from double sentence String") {
    assert(
      sentencesFromString("lorem ipsum.ciao") == List[Sentence](
        Sentence(List(Word("lorem"), Word("ipsum"))),
        Sentence(List(Word("ciao")))
      )
    )
  }

  test("wordCount of empty text is 0") {
    assert(wordCount(List()) == 0)
  }

  test("wordCount of single sentence of 2 words text is 2") {
    assert(wordCount(List(Sentence(List(Word("lorem"), Word("ipsum"))))) == 2)
  }

  test("wordCount of double sentence of 3 words in total text is 3") {
    assert(wordCount(List(Sentence(List(Word("lorem"), Word("ipsum"))), Sentence(List(Word("ciao"))))) == 3)
  }

  test("longest word in sentence of 1 word is length of the word") {
    assert(longestWordInSentence(Sentence(List(Word("ciao")))) == 4)
  }

  test("longest word in sentence of 'Hola todos' is length of 'todos'") {
    assert(longestWordInSentence(Sentence(List(Word("Hola"), Word("todos")))) == 5)
  }

}
