package adventofcode22

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import scala.annotation.tailrec
import Day8.NonEmptyMatrix._
import Day8.TreeVisibility._
import Day8.Visibility._

object Day8 {

  case class Height(value: Int)
  object Height {
    implicit val order: Order[Height] = Order.by(_.value)
  }

  case class Tree(height: Height)
  object Tree {
    def from(c: Char): Option[Tree] = if (c.isDigit) Some(Tree(Height(c.asDigit))) else None
  }

  sealed trait Visibility
  object Visibility {
    case object Visible extends Visibility
    case object Hidden extends Visibility
  }

  case class TreeVisibility(left: Visibility, top: Visibility, right: Visibility, bottom: Visibility)
  object TreeVisibility {
    val visibleFromAllSides: TreeVisibility =
      TreeVisibility(left = Visible, top = Visible, right = Visible, bottom = Visible)
    val hiddenFromAllSides: TreeVisibility =
      TreeVisibility(left = Hidden, top = Hidden, right = Hidden, bottom = Hidden)
  }

  case class NonEmptyMatrix[A](rows: NonEmptyList[NonEmptyList[A]]) {
    lazy val rotatedCW: NonEmptyMatrix[A] = transposed.vFlipped
    lazy val rotatedCCW: NonEmptyMatrix[A] = transposed.hFlipped
    lazy val transposed: NonEmptyMatrix[A] = NonEmptyMatrix(transpose(rows))
    lazy val vFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.map(_.reverse))
    lazy val hFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.reverse)
  }
  object NonEmptyMatrix {

    implicit def eq[A: Eq]: Eq[NonEmptyMatrix[A]] = derived.semiauto.eq
    implicit def functor: Functor[NonEmptyMatrix] = derived.semiauto.functor
    implicit def foldable: Foldable[NonEmptyMatrix] = derived.semiauto.foldable

    implicit def semigroupal: Semigroupal[NonEmptyMatrix] = new Semigroupal[NonEmptyMatrix] {
      override def product[A, B](fa: NonEmptyMatrix[A], fb: NonEmptyMatrix[B]): NonEmptyMatrix[(A, B)] =
        NonEmptyMatrix(fa.rows.zip(fb.rows).map { case (as, bs) => as.zip(bs) })
    }

    def transpose[A](rows: NonEmptyList[NonEmptyList[A]]): NonEmptyList[NonEmptyList[A]] = {
      def heads(rows: NonEmptyList[NonEmptyList[A]]): NonEmptyList[A] = rows.map(_.head)
      def tails(rows: NonEmptyList[NonEmptyList[A]]): List[List[A]] = rows.toList.map(_.tail)
      @tailrec
      def aux(acc: NonEmptyList[NonEmptyList[A]], rows: List[List[A]]): NonEmptyList[NonEmptyList[A]] =
        rows match {
          case Nil      => acc
          case Nil :: _ => acc
          case (_ :: _) :: _ =>
            val (newAcc, newRows) =
              rows
                .traverse(_.toNel)
                .flatMap(_.toNel)
                .map(rs => (acc :+ heads(rs), tails(rs)))
                .getOrElse((acc, List.empty))
            aux(newAcc, newRows)
        }
      aux(acc = NonEmptyList.of(heads(rows)), tails(rows))
    }

  }

  type Forest = NonEmptyMatrix[Tree]

  def getForestFrom(input: List[String]): Option[Forest] =
    input.headOption.flatMap { firstRow =>
      if (input.map(_.length).forall(_ == firstRow.length)) Some(firstRow) else None
    }.flatMap { _ =>
      input
        .traverse(_.toList.traverse(Tree.from))
        .flatMap(_.traverse(_.toNel))
        .flatMap(_.toNel)
        .map(NonEmptyMatrix.apply)
    }

  def getVisibilityFromLeft(row: NonEmptyList[Tree]): NonEmptyList[Visibility] =
    row.tail
      .foldLeft[(Tree, NonEmptyList[Visibility])]((row.head, NonEmptyList.of(Visible))) { case ((tallest, acc), t) =>
        if (t.height > tallest.height) (t, acc :+ Visible) else (tallest, acc :+ Hidden)
      }
      ._2

  def getVisibilityFromRight(row: NonEmptyList[Tree]): NonEmptyList[Visibility] =
    getVisibilityFromLeft(row.reverse).reverse

  def getLeftTreeVisibilities(forest: Forest): NonEmptyMatrix[Visibility] =
    NonEmptyMatrix(forest.rows.map(getVisibilityFromLeft))

  def getRightTreeVisibilities(forest: Forest): NonEmptyMatrix[Visibility] =
    NonEmptyMatrix(forest.rows.map(getVisibilityFromRight))

  def getTopTreeVisibilities(forest: Forest): NonEmptyMatrix[Visibility] =
    getRightTreeVisibilities(forest.rotatedCW).rotatedCCW

  def getBottomTreeVisibilities(forest: Forest): NonEmptyMatrix[Visibility] =
    getLeftTreeVisibilities(forest.rotatedCW).rotatedCCW

  def getTreeVisibilities(forest: Forest): NonEmptyMatrix[TreeVisibility] =
    (
      getLeftTreeVisibilities(forest),
      getTopTreeVisibilities(forest),
      getRightTreeVisibilities(forest),
      getBottomTreeVisibilities(forest)
    ).mapN(TreeVisibility.apply)

  def getTreesVisibleFromOutsideCount(forest: Forest): Long =
    getTreeVisibilities(forest).count(_ != hiddenFromAllSides)

  def getTreesVisibleFromOutsideCount(input: List[String]): Option[Long] =
    getForestFrom(input).map(getTreesVisibleFromOutsideCount)

}
