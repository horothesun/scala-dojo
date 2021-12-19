import scala.annotation.tailrec
import scala.math.abs

object Power {
  /*
    - def power(b: Int, e: Int): Int
    - fix signature
    - implement with tail recursion

    b = 0, e <= 0 := N.D.
    b = 0, e > 0 := 0
    b != 0, e < 0 := 1 / (b * ... * b, |e| times)
    b != 0, e = 0 := 1
    b != 0, e > 0 := b * ... * b, e times
   */
  def power(b: Int, e: Int): Option[BigDecimal] =
    if (b == 0) if (e <= 0) None else Some(0.0)
    else Some(powerNonZeroBase(b, e))

  def powerNonZeroBase(b: Int, e: Int): BigDecimal = {
    val bToAbsE = BigDecimal(powerNonZeroBaseNaturalExp(b, abs(e)).bigInteger)
    if (e >= 0) bToAbsE else 1.0 / bToAbsE
  }

  def powerNonZeroBaseNaturalExp(b: Int, e: Int): BigInt = {
    @tailrec
    def aux(b: Int, e: Int, acc: BigInt): BigInt =
      if (e == 0) acc else aux(b, e - 1, b * acc)
    aux(b, e, 1)
  }
}
