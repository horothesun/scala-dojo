object Fibonacci {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(acc1: Int, acc2: Int, n: Int): Int =
      n match {
        case 0 => acc1
        case 1 => acc2
        case _ => loop(acc2, acc1 + acc2, n - 1)
      }
    loop(0, 1, n)
  }
}
