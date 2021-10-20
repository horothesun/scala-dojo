import scala.annotation.tailrec

// Model a purely functional Stack

sealed trait Stack[T] {

  def toList: List[T] = this match {
    case EmptyStack() => List.empty
    case NonEmptyStack(top, tail) => top :: tail.toList
  }

  override def toString: String = {
    val bodyStr = this.toList.foldLeft("")((s, t) => if (s.isEmpty) s"$t" else s"$s > $t")
    s"[ $bodyStr ]"
  }
}

case class EmptyStack[T]() extends Stack[T]
case class NonEmptyStack[T](top: T, tail: Stack[T]) extends Stack[T]

object Stack {

  def push[T]: T => Stack[T] => Stack[T] = t => {
    case EmptyStack() => NonEmptyStack(t, EmptyStack())
    case s@NonEmptyStack(_, _) => NonEmptyStack(t, s)
  }

  def pop[T]: Stack[T] => Option[(T, Stack[T])] = {
    case EmptyStack() => None
    case NonEmptyStack(top, tail) => Some((top, tail))
  }

  def min[T](s: Stack[T])(implicit ord: Ordering[T]): Option[T] =
    s match {
      case EmptyStack() => None
      case NonEmptyStack(top, EmptyStack()) => Some(top)
      case NonEmptyStack(top, NonEmptyStack(sndTop, tail)) =>
        val topMin = ord.min(top, sndTop)
        min(tail)(ord).map(ord.min(topMin, _)).orElse(Some(topMin))
    }

  def safeMin[T](s: Stack[T])(implicit ord: Ordering[T]): Option[T] = {
    @tailrec
    def aux(acc: Option[T], s: Stack[T]): Option[T] = {
      s match {
        case EmptyStack() => acc
        case NonEmptyStack(top, tail) =>
          val newAcc = acc.map(ord.min(_, top)).orElse(Some(top))
          aux(newAcc, tail)
      }
    }
    aux(None, s)
  }
}
