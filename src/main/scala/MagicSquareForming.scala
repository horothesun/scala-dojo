import java.awt.ContainerOrderFocusTraversalPolicy

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
    newValue: Int,
    position: Position
  )

  case class Cost(value: Int)

  object Cost {
    implicit val ordering: Ordering[Cost] = Ordering.by[Cost, Int](_.value)
  }

  case class ReplaceResult(
    totalCost: Cost,
    square: Square
  )

  def replace(s: Square, rs: List[Replacement]): ReplaceResult = {
    val newRows = s.rows.map(_.clone)
    var totalCost = 0
    rs.foreach { r =>
      val i = r.position.rowIndex
      val j = r.position.columnIndex
      newRows(i)(j) = r.newValue
      totalCost += cost(r.newValue, s.rows(i)(j)).value
    }
    ReplaceResult(Cost(totalCost), Square(s.size, newRows))
  }

  def allValues(size: Int): List[Int] = (1 to size * size).toList

  def allPositions(size: Int): List[Position] = {
    val indexes = (0 until size).toList
    indexes.flatMap(i => indexes.map(j => Position(rowIndex = i, columnIndex = j)))
  }

  def allReplacements(size: Int): List[Replacement] =
    allValues(size).flatMap(v => allPositions(size).map(p => Replacement(newValue = v, position = p)))

  def cost(a: Int, b: Int): Cost = Cost(Math.abs(a - b))

  def isNoOp(r: Replacement, s: Square): Boolean =
    r.newValue == s.rows(r.position.rowIndex)(r.position.columnIndex)

  case class Result(
    replacements: List[Replacement],
    cost: Cost,
    square: Square
  )

  def minCostReplacements(s: Square): Result = {
    val possibleReplacements = allReplacements(s.size)
    def extendedResults(previous: Result): List[Result] =
      possibleReplacements
        .filterNot(r => isNoOp(r, previous.square))
        .map { r =>
          val replacements = previous.replacements.appended(r)
          val replaceResult = replace(previous.square, replacements)
          Result(
            replacements,
            replaceResult.totalCost,
            square = replaceResult.square
          )
        }

    val baseResult = Result(replacements = List.empty, Cost(0), s)
    val resultsByLength: Map[Int, List[Result]] =
      (1 to s.size * s.size).toList
        .foldLeft(Map(0 -> List(baseResult))) { case (acc, length) =>
          acc ++ acc(length - 1).map(res => (length, extendedResults(res))).toMap
        }

    resultsByLength.toList.flatMap { case (_, results) => results }
      .filter(result => result.square.isMagic)
      .minBy(result => result.cost)
  }

  def formingMagicSquare(s: Array[Array[Int]]): Int =
    minCostReplacements(Square(size = s.length, rows = s)).cost.value

}
