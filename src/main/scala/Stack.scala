// Model a purely functional Stack

sealed trait Stack[T]

object Stack {
  def push[T]: T => Stack[T] => Stack[T] = ???

  def pop[T]: Stack[T] => Option[(T, Stack[T])] = ???

  // implement with tail recursion
  def min[T: Ordering]: Stack[T] => Option[T] = ???
}
