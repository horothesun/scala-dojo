sealed trait FizzBuzz

object FizzBuzz {
  case object Fizz extends FizzBuzz
  case object Buzz extends FizzBuzz
  case object FizzBuzz extends FizzBuzz
  case class Other(i: Int) extends FizzBuzz

  def fizzBuzz(i: Int): FizzBuzz = {
    if (i % 3 == 0 && i % 5 == 0) FizzBuzz
    else if (i % 3 == 0) Fizz
    else if (i % 5 == 0) Buzz
    else Other(i)
  }

  def fizzBuzzes: Int => List[FizzBuzz] =
    n => (1 to n).toList.map(fizzBuzz)

  def render: FizzBuzz => String = {
    case Fizz => "Fizz"
    case Buzz => "Buzz"
    case FizzBuzz => "FizzBuzz"
    case Other(i) => s"Other(${i})"
  }
}
