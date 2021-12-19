import scala.annotation.tailrec

// Model a purely functional Stack

sealed trait Stack[+T] {

  def toList: List[T] = {

    @tailrec
    def toListAux(acc: List[T], s: Stack[T]): List[T] =
      s match {
        case EmptyStack               => acc
        case NonEmptyStack(top, tail) => toListAux(top :: acc, tail)
      }

    toListAux(List.empty, this)

  }

  override def toString: String = {
    val bodyStr = this.toList.foldLeft("")((s, t) => if (s.isEmpty) s"$t" else s"$s > $t")
    s"[ $bodyStr ]"
  }
}

case object EmptyStack                               extends Stack[Nothing]
case class NonEmptyStack[+T](top: T, tail: Stack[T]) extends Stack[T]

object Stack {

  def push[T, V >: T]: V => Stack[T] => Stack[V] = v => s => NonEmptyStack(v, s)

  def pop[T]: Stack[T] => Option[(T, Stack[T])] = {
    case EmptyStack               => None
    case NonEmptyStack(top, tail) => Some((top, tail))
  }

  // insert N elements starting from the leftmost
  def pushBatch[T, V >: T]: List[V] => Stack[T] => Stack[V] = vs =>
    stack => vs.foldLeft[Stack[V]](stack)((s, v) => push(v)(s))

  def popBatch[T]: Int => Stack[T] => (List[T], Stack[T]) = n =>
    stack =>
      List.fill(n)(()).foldLeft((List.empty[T], stack)) { case ((ts, s), _) =>
        pop(s).map { case (t, s1) => (ts :+ t, s1) }.getOrElse((ts, EmptyStack))
      }

  def min[T: Ordering](s: Stack[T]): Option[T] = s match {
    case EmptyStack                                      => None
    case NonEmptyStack(top, EmptyStack)                  => Some(top)
    case NonEmptyStack(top, NonEmptyStack(sndTop, tail)) =>
      val topMin = Ordering[T].min(top, sndTop)
      min(tail).map(Ordering[T].min(topMin, _)).orElse(Some(topMin))
  }

  def safeMin[T: Ordering](s: Stack[T]): Option[T] = {
    @tailrec
    def aux(acc: T, s: Stack[T]): T =
      s match {
        case EmptyStack               => acc
        case NonEmptyStack(top, tail) =>
          val newAcc = Ordering[T].min(acc, top)
          aux(newAcc, tail)
      }

    s match {
      case EmptyStack               => None
      case NonEmptyStack(top, tail) => Some(aux(top, tail))
    }

  }

  def fold[T, V](s: Stack[T], v: V, combine: (V, T) => V): V = {

    @tailrec
    def aux(acc: V, s: Stack[T]): V =
      s match {
        case EmptyStack               => acc
        case NonEmptyStack(top, tail) =>
          aux(combine(acc, top), tail)
      }

    s match {
      case EmptyStack               => v
      case NonEmptyStack(top, tail) => aux(combine(v, top), tail)
    }
  }

  def foldMin[T: Ordering](s: Stack[T]): Option[T] =
    s match {
      case EmptyStack               => None
      case NonEmptyStack(top, tail) => Some(fold(tail, top, Ordering[T].min[T]))
    }

  def size[T]: Stack[T] => Int =
    s => fold[T, Int](s, 0, { case (n, _) => n + 1 })

}
