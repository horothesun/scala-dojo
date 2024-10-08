package wordle

import Models._
import WordleLawsSuite._
import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary._

class WordleLawsSuite extends DisciplineSuite {

  checkAll("Word.FunctorLaws", FunctorTests[Word].functor[Int, Int, String])

}

object WordleLawsSuite {

  implicit def wordArb[A: Arbitrary]: Arbitrary[Word[A]] = Arbitrary(wordGen(arbitrary[A]))

  def wordGen[A](aGen: Gen[A]): Gen[Word[A]] =
    Gen
      .zip(aGen, aGen, aGen, aGen, aGen)
      .map[Word[A]] { case (a1, a2, a3, a4, a5) => Word(a1, a2, a3, a4, a5) }

}
