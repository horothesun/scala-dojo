import cats.data.NonEmptyList
import cats.implicits._

object AggregateWithZipper {

  def aggregatedList[A](as: List[A]): List[(A, Int)] =
    NonEmptyList
      .fromList(as)
      .fold[List[(A, Int)]](ifEmpty = List.empty)(aggregatedNelAux)

  def aggregatedNel[A](as: NonEmptyList[A]): NonEmptyList[(A, Int)] =
    aggregatedNelAux(as).toNel.get

  def aggregatedNelAux[A](as: NonEmptyList[A]): List[(A, Int)] =
    Zipper
      .fromNel(as)
      .coflatMap(aggregationStep)
      .toNel
      .collect { case Some(a) => a }

  private def aggregationStep[A](z: Zipper[A]): Option[(A, Int)] = {
    def res: (A, Int) = firstAggregated(z.toNelRight)
    z.moveLeftOption
      .map(_.extract)
      .fold[Option[(A, Int)]](ifEmpty = Some(res))(l => Option.when(l != z.extract)(res))
  }

  private def firstAggregated[A](as: NonEmptyList[A]): (A, Int) =
    (as.head, firstGroup(as).length)

  private def firstGroup[A](as: NonEmptyList[A]): NonEmptyList[A] =
    NonEmptyList(as.head, as.tail.takeWhile(_ == as.head))

}
