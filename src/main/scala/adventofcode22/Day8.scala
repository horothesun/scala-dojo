package adventofcode22

import cats._
import cats.data.{Nested, NonEmptyList}
import cats.syntax.all._

import scala.annotation.tailrec
import scala.math.Numeric.Implicits._
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

    def getLeftTopRightBottomTransform[B](
      transformRowLeft: NonEmptyList[A] => NonEmptyList[B],
      transformRowRight: NonEmptyList[A] => NonEmptyList[B]
    ): NonEmptyMatrix[(B, B, B, B)] = {
      def getLeftNemB(nemA: NonEmptyMatrix[A]): NonEmptyMatrix[B] = NonEmptyMatrix(nemA.rows.map(transformRowLeft))
      def getRightNemB(nemA: NonEmptyMatrix[A]): NonEmptyMatrix[B] = NonEmptyMatrix(nemA.rows.map(transformRowRight))
      def getTopNemB(nemA: NonEmptyMatrix[A]): NonEmptyMatrix[B] = getRightNemB(nemA.rotatedCW).rotatedCCW
      def getBottomNemB(nemA: NonEmptyMatrix[A]): NonEmptyMatrix[B] = getLeftNemB(nemA.rotatedCW).rotatedCCW
      (
        getLeftNemB(this),
        getTopNemB(this),
        getRightNemB(this),
        getBottomNemB(this)
      ).tupled
    }
  }
  object NonEmptyMatrix {

    implicit def eq[A: Eq]: Eq[NonEmptyMatrix[A]] = derived.semiauto.eq
    implicit def functor: Functor[NonEmptyMatrix] = derived.semiauto.functor
    implicit def foldable: Foldable[NonEmptyMatrix] = derived.semiauto.foldable

    // TODO: replace with Applicative[NonEmptyMatrix] instance!!! 🔥🔥🔥
    implicit def semigroupal: Semigroupal[NonEmptyMatrix] = new Semigroupal[NonEmptyMatrix] {
      override def product[A, B](fa: NonEmptyMatrix[A], fb: NonEmptyMatrix[B]): NonEmptyMatrix[(A, B)] =
        NonEmptyMatrix(fa.rows.zip(fb.rows).map { case (as, bs) => as.zip(bs) })
    }

//    implicit def applicative: Applicative[NonEmptyMatrix] = new Applicative[NonEmptyMatrix] {
//      type NestedNelNel[T] = Nested[NonEmptyList, NonEmptyList, T]
//      override def pure[A](x: A): NonEmptyMatrix[A] = NonEmptyMatrix(NonEmptyList.one(NonEmptyList.one(x)))
//      override def ap[A, B](ff: NonEmptyMatrix[A => B])(fa: NonEmptyMatrix[A]): NonEmptyMatrix[B] =
//        NonEmptyMatrix(Applicative[NestedNelNel].ap[A, B](Nested(ff.rows))(Nested(fa.rows)).value)
//    }

//    implicit def applicative: Applicative[NonEmptyMatrix] = new Applicative[NonEmptyMatrix] {
//      override def pure[A](x: A): NonEmptyMatrix[A] = NonEmptyMatrix(NonEmptyList.one(NonEmptyList.one(x)))
//      override def ap[A, B](ff: NonEmptyMatrix[A => B])(fa: NonEmptyMatrix[A]): NonEmptyMatrix[B] =
//        NonEmptyMatrix(ff.rows.zip(fa.rows).map { case (fs, as) => fs.zip(as).map { case (f, a) => f(a) } })
//    }

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

  def getTreeVisibilities(forest: Forest): NonEmptyMatrix[TreeVisibility] =
    forest
      .getLeftTopRightBottomTransform(
        transformRowLeft = getVisibilityFromLeft,
        transformRowRight = getVisibilityFromRight
      )
      .map((TreeVisibility.apply _).tupled)

  def getTreesVisibleFromOutsideCount(forest: Forest): Long =
    getTreeVisibilities(forest).count(_ != hiddenFromAllSides)

  def getTreesVisibleFromOutsideCount(input: List[String]): Option[Long] =
    getForestFrom(input).map(getTreesVisibleFromOutsideCount)

  case class Count(value: Int)
  object Count {
    implicit val order: Order[Count] = Order[Int].imap(Count.apply)(_.value)
    implicit val numeric: Numeric[Count] = Numeric[Int].imap(Count.apply)(_.value)
  }
  case class VisibleTreesCount(left: Count, top: Count, right: Count, bottom: Count) {
    def getScenicScore: Count = left * top * right * bottom
  }

  def getVisibleTreesCountOnLeft(row: NonEmptyList[Tree]): NonEmptyList[Count] =
    row.tail
      .foldLeft((NonEmptyList.one(row.head), NonEmptyList.one(Count(0)))) { case ((lts, counts), t) =>
        @tailrec
        def getCount(done: Boolean, count: Count, reversedLeftTrees: NonEmptyList[Tree]): Count =
          if (done) count
          else
            reversedLeftTrees match {
              case NonEmptyList(_, Nil) => getCount(done = true, Count(1) + count, reversedLeftTrees)
              case NonEmptyList(t1, t2 :: ts) =>
                getCount(done = t1.height >= t.height, Count(1) + count, NonEmptyList(t2, ts))
            }

        val c = getCount(done = false, Count(0), reversedLeftTrees = lts.reverse)
        (lts :+ t, counts :+ c)
      }
      ._2

  def getVisibleTreesCountOnRight(row: NonEmptyList[Tree]): NonEmptyList[Count] =
    getVisibleTreesCountOnLeft(row.reverse).reverse

  def getVisibleTreeCounts(forest: Forest): NonEmptyMatrix[VisibleTreesCount] =
    forest
      .getLeftTopRightBottomTransform(
        transformRowLeft = getVisibleTreesCountOnLeft,
        transformRowRight = getVisibleTreesCountOnRight
      )
      .map((VisibleTreesCount.apply _).tupled)

  def getMaxVisibleTreeCount(forest: Forest): Option[Count] =
    getVisibleTreeCounts(forest).map(_.getScenicScore).maximumOption

  def getMaxVisibleTreeCount(input: List[String]): Option[Count] =
    getForestFrom(input).flatMap(getMaxVisibleTreeCount)

}
