import scala.math.Ordering.Implicits._

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

    lazy val flattenedValues: List[Int] = rows.toList.flatMap(_.toList)

    lazy val isMagic: Boolean = {
      val topLeftBottomRightDiagonalSum = topLeftBottomRightDiagonal.sum
      flattenedValues.sorted == allValues(size) &&
      (
        rows.map(r => r.sum) ++
          columns.map(c => c.sum) ++
          Array(topLeftBottomRightDiagonalSum, topRightBottomLeftDiagonal.sum)
      ).forall(_ == topLeftBottomRightDiagonalSum)
    }

    lazy val magicConstant: Option[Int] =
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
            this.flattenedValues == that.flattenedValues
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

  def allReplacements(size: Int): List[Replacement] = {
    val possibleValues = allValues(size)
    allPositions(size).flatMap(p => possibleValues.map(v => Replacement(newValue = v, position = p)))
  }

  def cost(a: Int, b: Int): Cost = Cost(Math.abs(a - b))

  def isNoOp(r: Replacement, s: Square): Boolean =
    r.newValue == s.rows(r.position.rowIndex)(r.position.columnIndex)

  case class Candidate(
    replacements: List[Replacement],
    cost: Cost,
    square: Square
  )

  def minCostReplacements(s: Square): Candidate = {
    val possibleReplacements = allReplacements(s.size)
    def extendedCandidates(start: Square, c: Candidate): List[Candidate] =
      possibleReplacements
        .filterNot(r => isNoOp(r, c.square))
        .map { r =>
          val replacements = c.replacements.appended(r)
          val replaceResult = replace(start, replacements)
          Candidate(replacements, replaceResult.totalCost, replaceResult.square)
        }

    val baseCandidate = Candidate(List.empty[Replacement], Cost(0), s)
    val baseMinMagicCost: Option[Cost] = if (baseCandidate.square.isMagic) Some(baseCandidate.cost) else None
    val (_, candidatesByLength) = (1 to s.size * s.size).toList
      .foldLeft((baseMinMagicCost, Map(0 -> List(baseCandidate)))) { case ((accMagicMinCost, acc), length) =>
        def isCandidateValid(c: Candidate): Boolean = accMagicMinCost.fold(true)(minCost => c.cost < minCost)
        val newCandidates = acc(length - 1).filter(isCandidateValid).flatMap(c => extendedCandidates(s, c))
        val newAcc = acc ++ Map(length -> newCandidates)
        val newAccMagicMinCost = (
          newCandidates.filter(_.square.isMagic).map(_.cost) ++ accMagicMinCost.toList
        ).minOption
        (newAccMagicMinCost, newAcc)
      }

    val magicCandidates = candidatesByLength.toList.flatMap { case (_, cs) => cs }.filter(_.square.isMagic)
    magicCandidates.minBy(_.cost)
  }

  def formingMagicSquare(s: Array[Array[Int]]): Int =
    minCostReplacements(Square(size = s.length, rows = s)).cost.value

}
