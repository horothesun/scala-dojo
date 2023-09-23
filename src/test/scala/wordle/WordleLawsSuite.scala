package wordle

import cats.laws.discipline.{FunctorTests, SemigroupKTests}
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Wordle.{Guess, Guesser, Word, WordPos}
import Wordle.WordPos._
import WordleLawsSuite._

class WordleLawsSuite extends DisciplineSuite {

  checkAll("Word.FunctorLaws", FunctorTests[Word].functor[Int, Int, String])

//  checkAll("Guesser.SemigroupKLaws", SemigroupKTests[Guesser].semigroupK[Int])

}

object WordleLawsSuite {

  implicit def wordArb[A: Arbitrary]: Arbitrary[Word[A]] = Arbitrary(wordGen(arbitrary[A]))

  implicit def guesserArb[A: Arbitrary]: Arbitrary[Guesser[A]] = Arbitrary(guesserGen(arbitrary[A]))

  def wordGen[A](aGen: Gen[A]): Gen[Word[A]] =
    Gen
      .zip(aGen, aGen, aGen, aGen, aGen)
      .map[Word[A]] { case (a1, a2, a3, a4, a5) => Word(a1, a2, a3, a4, a5) }

  def guesserGen[A](aGen: Gen[A]): Gen[Guesser[A]] =
    Gen.lzy(
      Gen.oneOf(
        emptyGuesserGen[A],
        constGuesserGen[A](aGen),
        allGuesserGen[A],
        absentFromWordGuesserGen(aGen),
        wrongPositionGuesserGen(aGen),
        matchingPositionGuesserGen(aGen),
        andGuesserGen(aGen)
      )
    )
  def emptyGuesserGen[A]: Gen[Guesser[A]] = Gen.const(Guesser.Empty[A]())
  def constGuesserGen[A](aGen: Gen[A]): Gen[Guesser[A]] = guessGen(aGen).map(Guesser.Const.apply)
  def allGuesserGen[A]: Gen[Guesser[A]] = Gen.const(Guesser.All[A]())
  def absentFromWordGuesserGen[A](aGen: Gen[A]): Gen[Guesser[A]] = aGen.map(Guesser.AbsentFromWord.apply)
  def wrongPositionGuesserGen[A](aGen: Gen[A]): Gen[Guesser[A]] =
    Gen.zip(aGen, wordPosGen).map { case (a, pos) => Guesser.WrongPosition(a, pos) }
  def matchingPositionGuesserGen[A](aGen: Gen[A]): Gen[Guesser[A]] =
    Gen.zip(aGen, wordPosGen).map { case (a, pos) => Guesser.MatchingPosition(a, pos) }
  def andGuesserGen[A](aGen: Gen[A]): Gen[Guesser[A]] =
    Gen.zip(guesserGen(aGen), guesserGen(aGen)).map { case (g1, g2) => Guesser.And(g1, g2) }

  def guessGen[A](aGen: Gen[A]): Gen[Guess[A]] = wordGen(aGen).map(Guess.apply)

  def wordPosGen: Gen[WordPos] = Gen.oneOf[WordPos](Pos1, Pos2, Pos3, Pos4, Pos5)

}
