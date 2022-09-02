/*
  We define a magic square to be an `n x n` matrix of distinct positive integers from `1` to `n^2`
  where the sum of any row, column, or diagonal of length `n` is always equal to the same number: the magic constant.
  You will be given a `3 x 3` matrix `s` of integers in the inclusive range `[1, 9]`.
  We can convert any digit `a` to any other digit `b` in the range `[1, 9]` at cost of `|a - b|`.
  Given `s`, convert it into a magic square at minimal cost. Print this cost on a new line.

  Note: The resulting magic square must contain distinct integers in the inclusive range `[1, 9]`.
 */
object MagicSquareForming {

  case class Square(
    size: Int,
    rows: Array[Array[Int]]
  ) {
    def isMagic: Boolean = {
      val topLeftBottomRightDiagonalSum = topLeftBottomRightDiagonal.sum
      (columns.map(c => c.sum) ++ Array(topLeftBottomRightDiagonalSum, topRightBottomLeftDiagonal.sum))
        .forall(_ == topLeftBottomRightDiagonalSum)
    }

    def magicConstant: Option[Int] =
      if (isMagic) Some(topLeftBottomRightDiagonal.sum) else None

    lazy val columns: Array[Array[Int]] =
      (0 until size).toArray.map(colIdx => rows.map(r => r(colIdx)))

    lazy val topLeftBottomRightDiagonal: Array[Int] =
      (0 until size).toArray.map(i => rows(i)(i))

    lazy val topRightBottomLeftDiagonal: Array[Int] =
      (0 until size).toArray.map(i => rows(i)(size - 1 - i))

    override def equals(that: Any): Boolean =
      that match {
        case that: Square =>
          that.canEqual(this) &&
            this.size == that.size &&
            this.rows.toList.map(_.toList) == that.rows.toList.map(_.toList)
        case _ => false
      }

    override def canEqual(a: Any): Boolean = a.isInstanceOf[Square]
  }

  case class Position(
    rowIndex: Int,
    columnIndex: Int
  )

  case class Replacement(
    position: Position,
    newValue: Int
  )

  case class Cost(value: Int)

  case class ReplaceResult(
    costs: List[Cost],
    resultSquare: Square
  )

  def replace(s: Square, rs: List[Replacement]): ReplaceResult = {
    val newRows = s.rows.map(_.clone)
    var costs = Array.empty[Cost]
    rs.foreach { r =>
      val i = r.position.rowIndex
      val j = r.position.columnIndex
      newRows(i)(j) = r.newValue
      costs = costs.appended(cost(r.newValue, s.rows(i)(j)))
    }
    ReplaceResult(costs.toList, Square(s.size, newRows))
  }

  def allPossibleValues(size: Int): List[Int] = (1 to size * size).toList

  def cost(a: Int, b: Int): Cost = Cost(Math.abs(a - b))

}
