// Model a purely functional Stack

sealed trait Stack[T]

case class StackDefault[T](elements: List[T]) extends Stack[T]

object Stack {
  def push[T]: T => Stack[T] => Stack[T] = e => {
    case StackDefault(elements) => StackDefault(e +: elements)
  }

  def pop[T]: Stack[T] => Option[(T, Stack[T])] = {
    case StackDefault(elements) => elements match {
      case ::(head, next) => Some((head, StackDefault(next)))
      case Nil => None
    }
  }

  // implement with tail recursion
  def min[T: Ordering]: Stack[T] => Option[T] = ???
}
