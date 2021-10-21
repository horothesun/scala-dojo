import scala.annotation.tailrec

// Model a purely functional Stack

sealed trait Stack[T] {

  def toList: List[T] = {

    @tailrec
    def toListAux(acc : List[T], s : Stack[T]) : List[T] =
      s match {
        case EmptyStack() => acc
        case NonEmptyStack(top, tail) => toListAux( top :: acc, tail)
      }

    toListAux(List.empty, this)

  }

  override def toString: String = {
    val bodyStr = this.toList.foldLeft("")((s, t) => if (s.isEmpty) s"$t" else s"$s > $t")
    s"[ $bodyStr ]"
  }
}

case class EmptyStack[T]() extends Stack[T]
case class NonEmptyStack[T](top: T, tail: Stack[T]) extends Stack[T]

object Stack {

  def push[T]: T => Stack[T] => Stack[T] = t => s => NonEmptyStack(t, s)

  def pop[T]: Stack[T] => Option[(T, Stack[T])] = {
    case EmptyStack() => None
    case NonEmptyStack(top, tail) => Some((top, tail))
  }

  def min[T : Ordering](s : Stack[T]): Option[T] = s match {
      case EmptyStack() => None
      case NonEmptyStack(top, EmptyStack()) => Some(top)
      case NonEmptyStack(top, NonEmptyStack(sndTop, tail)) =>
        val topMin = Ordering[T].min(top, sndTop)
        min(tail).map(Ordering[T].min(topMin, _)).orElse(Some(topMin))
    }

  def safeMin[T: Ordering](s : Stack[T]): Option[T] = {
    @tailrec
    def aux(acc: T, s: Stack[T]): T =
      s match {
        case EmptyStack() => acc
        case NonEmptyStack(top, tail) =>
          val newAcc = Ordering[T].min(acc, top)
          aux(newAcc, tail)
      }

    s match {
      case EmptyStack() => None
      case NonEmptyStack(top, tail) => Some(aux(top, tail))
    }

  }

  def fold[T,V](s : Stack[T], v : V, combine : (V,T) => V) : V = {

    @tailrec
    def aux(acc: V, s: Stack[T]): V =
      s match {
        case EmptyStack() => acc
        case NonEmptyStack(top, tail) =>
          aux(combine(acc, top), tail)
      }

    s match {
      case EmptyStack() => v
      case NonEmptyStack(top, tail) => aux(combine(v,top), tail)
    }
  }

  def foldMin[T: Ordering](s : Stack[T]): Option[T] = {
    s match {
      case EmptyStack() => None
      case NonEmptyStack(top, tail) =>  Some(fold(tail, top, Ordering[T].min[T]))
    }
  }

}
