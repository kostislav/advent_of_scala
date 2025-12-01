package cz.judas.jan.advent

import scala.annotation.targetName
import scala.math.{abs, floorMod}


case class Position(row: Int, column: Int):
  @targetName("plus")
  def +(offset: RelativePosition): Position =
    Position(row + offset.rowOffset, column + offset.columnOffset)

  @targetName("minus")
  def -(offset: RelativePosition): Position =
    Position(row - offset.rowOffset, column - offset.columnOffset)

  @targetName("minus")
  def -(other: Position): RelativePosition =
    RelativePosition(row - other.row, column - other.column)

  def mod(rows: Int, columns: Int): Position =
    Position(floorMod(row, rows), floorMod(column, columns))

  def withColumn(transformation: Int => Int): Position =
    Position(row, transformation(column))


case class RelativePosition(rowOffset: Int, columnOffset: Int):
  @targetName("times")
  def *(multiplier: Int): RelativePosition =
    RelativePosition(rowOffset * multiplier, columnOffset * multiplier)

  @targetName("plus")
  def +(other: RelativePosition): RelativePosition =
    RelativePosition(rowOffset + other.rowOffset, columnOffset + other.columnOffset)

  @targetName("invert")
  def unary_- : RelativePosition =
    RelativePosition(-rowOffset, -columnOffset)

  def rotateRight: RelativePosition =
    RelativePosition(columnOffset, -rowOffset)

  def rotateLeft: RelativePosition =
    RelativePosition(-columnOffset, rowOffset)

  def manhattanDistance: Int =
    abs(rowOffset) + abs(columnOffset)

object RelativePosition:
  val HERE: RelativePosition = RelativePosition(0, 0)
  val UP: RelativePosition = RelativePosition(-1, 0)
  val DOWN: RelativePosition = RelativePosition(1, 0)
  val LEFT: RelativePosition = RelativePosition(0, -1)
  val RIGHT: RelativePosition = RelativePosition(0, 1)
  val UP_RIGHT: RelativePosition = UP + RIGHT
  val UP_LEFT: RelativePosition = UP + LEFT
  val DOWN_RIGHT: RelativePosition = DOWN + RIGHT
  val DOWN_LEFT: RelativePosition = DOWN + LEFT

  def horizontalDirections: Seq[RelativePosition] =
    Seq(DOWN, RIGHT, UP, LEFT)

  def diagonalDirections: Seq[RelativePosition] =
    Seq(UP_RIGHT, UP_LEFT, DOWN_RIGHT, DOWN_LEFT)

  def allDirections: Seq[RelativePosition] =
    horizontalDirections ++ diagonalDirections

  private def fromTuples(tuples: Seq[(Int, Int)]): Seq[RelativePosition] =
    tuples.map(tuple => RelativePosition(tuple._1, tuple._2))


case class DirectionalPosition(position: Position, direction: RelativePosition):
  def turnLeft: DirectionalPosition =
    copy(direction = direction.rotateLeft)

  def turnRight: DirectionalPosition =
    copy(direction = direction.rotateRight)

  def walk(howFar: Int): DirectionalPosition =
    copy(position = position + direction * howFar)


class Array2d private(val numRows: Int, val numColumns: Int, lookup: Position => Option[Char]):
  def get(position: Position): Option[Char] =
    lookup(position)

  def apply(position: Position): Char =
    get(position).get

  def indices: Iterator[Position] =
    (0 until numRows)
      .iterator
      .flatMap(row => (0 until numColumns).map(column => Position(row, column)))
      .filter(contains)

  def columnIndices: Seq[Int] =
    0 until numColumns

  def rowIndices: Seq[Int] =
    0 until numRows

  def entries: Iterator[(Position, Char)] =
    indices.flatMap(position => get(position).map((position, _)))

  def contains(position: Position): Boolean =
    lookup(position).isDefined

  def neighbors(position: Position): Seq[Position] =
    RelativePosition.horizontalDirections.map(position + _).filter(contains)

  def positionOfOnly(c: Char): Position =
    indices.filter(apply(_) == c).getOnlyElement

  def positionsOf(c: Char): Set[Position] =
    indices.filter(apply(_) == c).toSet

  def ignoring(value: Char): Array2d =
    Array2d(numRows, numColumns, position => get(position).filter(_ != value))

object Array2d:
  def fromRows(rows: IndexedSeq[String]): Array2d = {
    val numRows = rows.size
    val numColumns = rows(0).length
    Array2d(
      numRows,
      numColumns,
      position =>
        if position.row >= 0 && position.row < numRows && position.column >= 0 && position.column < numColumns then
          Some(rows(position.row).charAt(position.column))
        else
          None
    )
  }

  def fromRows(rows: String*): Array2d =
    fromRows(rows.toIndexedSeq)


enum Turn:
  @pattern("R") case Right
  @pattern("L") case Left

  def apply(direction: DirectionalPosition): DirectionalPosition =
    this match
      case Turn.Right => direction.turnRight
      case Turn.Left => direction.turnLeft
