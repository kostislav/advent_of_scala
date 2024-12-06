package cz.judas.jan.advent

import scala.annotation.targetName


case class Position(row: Int, column: Int):
  @targetName("plus")
  def +(offset: RelativePosition): Position =
    Position(row + offset.rowOffset, column + offset.columnOffset)

  @targetName("minus")
  def -(offset: RelativePosition): Position =
    Position(row - offset.rowOffset, column - offset.columnOffset)


case class RelativePosition(rowOffset: Int, columnOffset: Int):
  @targetName("times")
  def *(multiplier: Int): RelativePosition =
    RelativePosition(rowOffset * multiplier, columnOffset * multiplier)

object RelativePosition:
  def allDirections: Seq[RelativePosition] =
    fromTuples(Seq((1, 0), (0, 1), (1, 1), (1, -1), (-1, 0), (0, -1), (-1, -1), (-1, 1)))

  def diagonalDirections: Seq[RelativePosition] =
    fromTuples(Seq((1, 1), (1, -1), (-1, -1), (-1, 1)))

  private def fromTuples(tuples: Seq[(Int, Int)]): Seq[RelativePosition] =
    tuples.map(tuple => RelativePosition(tuple._1, tuple._2))


class Array2d private(rows: IndexedSeq[String], val numRows: Int, val numColumns: Int):
  def get(position: Position): Option[Char] =
    if contains(position) then
      Some(rows(position.row).charAt(position.column))
    else
      None

  def indices: Iterator[Position] =
    (0 until numRows).iterator.flatMap(column => (0 until numColumns).map(row => Position(row, column)))

  def contains(position: Position): Boolean =
    position.row >= 0 && position.row < numRows && position.column >= 0 && position.column < numColumns

object Array2d:
  def fromInput(input: InputData): Array2d =
    val rows = input.lines.toIndexedSeq
    Array2d(rows, rows.size, rows(0).length)
