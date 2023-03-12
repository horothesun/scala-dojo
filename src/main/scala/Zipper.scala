import cats.Comonad
import cats.data.NonEmptyList

// https://www.47deg.com/blog/game-of-life-scala/#enter-the-zipper-4
case class Zipper[A](left: LazyList[A], focus: A, right: LazyList[A]) {

  def moveRightOption: Option[Zipper[A]] =
    right match {
      case nextRight #:: rights => Some(Zipper(focus #:: left, nextRight, rights))
      case _                    => None
    }

  def moveLeftOption: Option[Zipper[A]] =
    left match {
      case nextLeft +: lefts => Some(Zipper(lefts, nextLeft, focus +: right))
      case _                 => None
    }

  def moveRight: Zipper[A] =
    if (right.isEmpty) this
    else Zipper(focus #:: left, right.head, right.tail)

  def moveLeft: Zipper[A] =
    if (left.isEmpty) this
    else Zipper(left.tail, left.head, focus #:: right)

  def toNelLeft: NonEmptyList[A] = {
    val ll = (focus #:: left).reverse
    NonEmptyList(ll.head, ll.tail.toList)
  }

  def toNel: NonEmptyList[A] = {
    val ll = (focus #:: left).reverse ++ right
    NonEmptyList(ll.head, ll.tail.toList)
  }

  def toNelRight: NonEmptyList[A] =
    NonEmptyList(focus, right.toList)

  def extract(implicit w: Comonad[Zipper]): A = w.extract(this)

  def coflatMap[B](f: Zipper[A] => B)(implicit w: Comonad[Zipper]): Zipper[B] =
    w.coflatMap(this)(f)

  def coflatten(implicit w: Comonad[Zipper]): Zipper[Zipper[A]] =
    w.coflatten(this)

  def map[B](f: A => B)(implicit w: Comonad[Zipper]): Zipper[B] =
    w.map(this)(f)

}

object Zipper {

  implicit val comonad: Comonad[Zipper] = new Comonad[Zipper] {

    override def extract[A](x: Zipper[A]): A = x.focus

    override def coflatMap[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
      map(coflatten(fa))(f)

    override def coflatten[A](fa: Zipper[A]): Zipper[Zipper[A]] =
      Zipper[Zipper[A]](lefts(fa), fa, rights(fa))

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      Zipper(fa.left.map(f), f(fa.focus), fa.right.map(f))

    private def lefts[A](fa: Zipper[A]): LazyList[Zipper[A]] =
      unfold(fa)(z => z.moveLeftOption.map(x => (x, x)))

    private def rights[A](fa: Zipper[A]): LazyList[Zipper[A]] =
      unfold(fa)(z => z.moveRightOption.map(x => (x, x)))

    private def unfold[A, B](a: A)(f: A => Option[(B, A)]): LazyList[B] =
      f(a) match {
        case None         => LazyList.empty
        case Some((b, a)) => b #:: unfold(a)(f)
      }

  }

  def fromNel[A](nel: NonEmptyList[A]): Zipper[A] =
    Zipper(LazyList.empty, nel.head, LazyList.from(nel.tail))

}
