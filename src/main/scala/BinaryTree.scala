sealed trait BinaryTree[+T] {

  def toList: List[T] = ???

  def fold[S >: T, R]: S => (T => R) => R = ???

  def reversed[S >: T]: BinaryTree[T] = ???
}

object Nil extends BinaryTree[Nothing] {
  override def toString: String = "Nil"
}
case class Node[T](element: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T] {
  override def toString: String = s"[$element | l: $left | r: $right]"
}

object BinaryTree
