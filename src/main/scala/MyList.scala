import scala.collection.immutable.{AbstractSeq, LinearSeq}

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
    case MyNil             => None
    case MyCons(_, asTail) => Some(asTail)
  }

}
