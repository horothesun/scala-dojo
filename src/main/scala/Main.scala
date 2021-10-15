object Main extends App {
  println("Welcome to the Scala Dojo!")
  println(Power.powerNonZeroBaseNaturalExp(-565765, 19889785))
}

class Ciao {
  def methodA: Int = 4

  def withMetric[A, B](name: String, block: A => B): A => B = ???
}

class Ciao2(ciao: Ciao) {
  import ciao.withMetric
  def methodB: Int => String = withMetric("pippo", { i: Int => s"${i + 1}" })
}
