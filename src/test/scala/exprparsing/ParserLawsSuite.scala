package exprparsing

import cats.laws.discipline.{FunctorTests, MonadTests}
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary._
import ParserLawsSuite._
//import exprparsing.ExprParsing.ParserOps

class ParserLawsSuite extends DisciplineSuite {

//  checkAll("Parser.FunctorLaws", FunctorTests[Parser].functor[Char, Int, String])
//  checkAll("Parser.MonadLaws", MonadTests[Parser].monad[Int, Int, Char])

}
object ParserLawsSuite {

//  implicit val parserIntArb: Arbitrary[Parser[Int]] = Arbitrary(parserIntGen)
//  implicit val parserCharArb: Arbitrary[Parser[Char]] = Arbitrary(parserCharGen)

//  def parserIntGen: Gen[Parser[Int]] = Gen.const(ParserOps.natural)
//  def parserCharGen: Gen[Parser[Char]] = Gen.const(ParserOps.digitChar)

}
