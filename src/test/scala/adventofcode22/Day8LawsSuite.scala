package adventofcode22

import cats.data.NonEmptyList
import cats.laws.discipline.SemigroupalTests
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary._
import Day8._
import Day8LawsSuite._

class Day8LawsSuite extends DisciplineSuite {

  checkAll("NonEmptyMatrix.SemigroupalLaws", SemigroupalTests[NonEmptyMatrix].semigroupal[Int, Int, String])

}
object Day8LawsSuite {

  implicit def nonEmptyMatrixArb[A: Arbitrary]: Arbitrary[NonEmptyMatrix[A]] =
    Arbitrary(nonEmptyMatrixGen(arbitrary[A]))

  def nonEmptyMatrixGen[A](aGen: Gen[A]): Gen[NonEmptyMatrix[A]] =
    Gen.zip(Gen.chooseNum(1, 10), Gen.chooseNum(1, 10)).flatMap { case (r, c) => nonEmptyMatrixGen(r, c, aGen) }
  def nonEmptyMatrixGen[A](rows: Int, columns: Int, aGen: Gen[A]): Gen[NonEmptyMatrix[A]] =
    nelGen(rows, nelGen(columns, aGen)).map(NonEmptyMatrix.apply)

  def nelGen[A](n: Int, aGen: Gen[A]): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(n - 1, aGen)).map { case (a, as) => NonEmptyList(a, as) }

}
