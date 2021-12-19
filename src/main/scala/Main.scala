object Main extends App {
  println("Welcome to the Scala Dojo!")
  println(Power.power(0, 0))

  val as = List[Int]()
  val n = 1
  println(s"${as.take(n)}, ${as.drop(n)}")
  println(s"Fibonacci numbers: ${(0 to 9).map(Fibonacci.fib).mkString(", ")}, ...")
}

class Ciao {
  def methodA: Int = 4

  def withMetric[A, B](name: String, block: A => B): A => B = ???
}

class Ciao2(ciao: Ciao) {
  import ciao.withMetric
  def methodB: Int => String = withMetric("pippo", { i: Int => s"${i + 1}" })
}
