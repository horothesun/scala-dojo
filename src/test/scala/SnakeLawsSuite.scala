import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary._
import ClassicSnake.Snake
import ClassicSnake.Snake._
import SnakeLawsSuite._

class SnakeLawsSuite extends DisciplineSuite {

  checkAll("Snake.FunctorLaws", FunctorTests[Snake].functor[Int, Int, String])

}

object SnakeLawsSuite {

  implicit def snakeArb[A: Arbitrary]: Arbitrary[Snake[A]] = Arbitrary(snakeGen(arbitrary[A]))

  def snakeGen[A](aGen: Gen[A]): Gen[Snake[A]] =
    Gen.lzy(
      Gen.oneOf(
        dotGen(aGen),
        grownForwardGen(aGen),
        grownLeftGen(aGen),
        grownRightGen(aGen)
      )
    )
  def dotGen[A](aGen: Gen[A]): Gen[Snake[A]] = aGen.map(a => Dot[A](a))
  def grownForwardGen[A](aGen: Gen[A]): Gen[Snake[A]] = headTailGen(aGen).map { case (a, s) => GrownForward(a, s) }
  def grownLeftGen[A](aGen: Gen[A]): Gen[Snake[A]] = headTailGen(aGen).map { case (a, s) => GrownLeft(a, s) }
  def grownRightGen[A](aGen: Gen[A]): Gen[Snake[A]] = headTailGen(aGen).map { case (a, s) => GrownRight(a, s) }
  def headTailGen[A](aGen: Gen[A]): Gen[(A, Snake[A])] = Gen.zip(aGen, snakeGen(aGen))

}
