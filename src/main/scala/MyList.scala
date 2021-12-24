sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil              => 0
    case MyCons(head, tail) => head + sum(tail)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil              => 1.0
    case MyCons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  def tail[A](as: MyList[A]): Option[MyList[A]] = as match {
    case MyNil         => None
    case MyCons(_, xs) => Some(xs)
  }

  def length[A](as: MyList[A]): Int = {
    @annotation.tailrec
    def loop(acc: Int, as: MyList[A]): Int = as match {
      case MyNil           => acc
      case MyCons(_, tail) => loop(1 + acc, tail)
    }
    loop(0, as)
  }

  def setHead[A](newHead: A, as: MyList[A]): Option[MyList[A]] =
    tail(as).map(MyCons(newHead, _))

  @annotation.tailrec
  def drop[A](as: MyList[A], n: Int): MyList[A] =
    if (n <= 0) as
    else
      as match {
        case MyNil           => MyNil
        case MyCons(_, tail) => drop(tail, n - 1)
      }

}
