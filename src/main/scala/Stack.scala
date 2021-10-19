// Model a purely functional Stack

sealed trait Stack[T] {

  def toList: List[T] = this match {
    case EmptyStack() => List.empty
    case NonEmptyStack(top, tail) => top :: tail.toList
  }

  override def toString: String = {
    val bodyStr = this.toList.foldLeft("")((s, t) => if (s.isEmpty) s"$t" else s"$s > $t")
    s"[ ${bodyStr} ]"
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

  // implement with tail recursion
  def min[T: Ordering]: Stack[T] => Option[T] = ???
}
