object Sorting {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(head: A, tail: List[A]): Boolean =
      if (tail.isEmpty) true
      else {
        val newHead = tail.head
        if (ordered(head, newHead)) loop(newHead, tail.drop(1))
        else false
      }
    if (as.isEmpty) true else loop(as(0), as.toList.drop(1))
  }
}
