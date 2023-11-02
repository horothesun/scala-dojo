package adventofcode22

import scala.util.Try

object ListOps {

  def splitListBy[A](elem: A)(as: List[A]): List[List[A]] =
    (as :+ elem)
      .foldLeft((List.empty[List[A]], List.empty[A])) { case ((res, acc), next) =>
        if (next == elem) (res :+ acc, List.empty) else (res, acc :+ next)
      }
      ._1

  def updatedOption[A](index: Int, elem: A)(as: List[A]): Option[List[A]] =
    Try(as.updated(index, elem)).toOption

}
