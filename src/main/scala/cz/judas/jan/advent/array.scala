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

  def horizontalDirections: Seq[RelativePosition] =
    fromTuples(Seq((1, 0), (0, 1), (-1, 0), (0, -1)))

  def diagonalDirections: Seq[RelativePosition] =
    fromTuples(Seq((1, 1), (1, -1), (-1, -1), (-1, 1)))

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


class Array2d private(rows: IndexedSeq[String], val numRows: Int, val numColumns: Int):
  def get(position: Position): Option[Char] =
    if contains(position) then
      Some(rows(position.row).charAt(position.column))
    else
      None

  def apply(position: Position): Char =
    get(position).get

  def indices: Iterator[Position] =
    (0 until numRows).iterator.flatMap(row => (0 until numColumns).map(column => Position(row, column)))

  def columnIndices: Seq[Int] =
    0 until numColumns

  def rowIndices: Seq[Int] =
    0 until numRows

  def entries: Iterator[(Position, Char)] =
    indices.map(position => (position, this(position)))

  def contains(position: Position): Boolean =
    position.row >= 0 && position.row < numRows && position.column >= 0 && position.column < numColumns

  def neighbors(position: Position): Seq[Position] =
    RelativePosition.horizontalDirections.map(position + _).filter(contains)

  def positionOfOnly(c: Char): Position =
    indices.filter(apply(_) == c).getOnlyElement

  def positionsOf(c: Char): Set[Position] =
    indices.filter(apply(_) == c).toSet

object Array2d:
  def fromRows(rows: IndexedSeq[String]): Array2d =
    Array2d(rows, rows.size, rows(0).length)

  def fromRows(rows: String*): Array2d =
    fromRows(rows.toIndexedSeq)
