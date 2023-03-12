import scala.annotation.tailrec

object ExtraLongFactorials {

  def extraLongFactorials(n: Int): BigInt = {
    @tailrec
    def aux(acc: BigInt, i: Int): BigInt = if (i > n) acc else aux(i * acc, 1 + i)

    aux(1, 1)
  }

}
