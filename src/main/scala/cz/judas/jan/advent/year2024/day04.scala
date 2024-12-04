package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, cartesianProduct}

import scala.annotation.targetName

object Day04:
  def part1(input: InputData): Int =
    val wordSearch = WordSearch.fromInput(input)
    RelativePosition.allDirections
      .map: direction =>
        wordSearch.indices
          .count: startingPosition =>
            wordSearch.wordAt(startingPosition, direction, 4) == "XMAS"
      .sum

  def part2(input: InputData): Int =
    val wordSearch = WordSearch.fromInput(input)
    val diagonalCombinations = RelativePosition.diagonalDirections.cartesianProduct(onlyDifferent = true)

    wordSearch.indices
      .count: startingPosition =>
        diagonalCombinations.exists { case (first, second) =>
          wordSearch.wordAt(startingPosition - first, first, 3) == "MAS"
            && wordSearch.wordAt(startingPosition - second, second, 3) == "MAS"
        }


class WordSearch(letters: Array2d):
  def indices: Iterator[Position] =
    letters.indices

  def wordAt(position: Position, direction: RelativePosition, length: Int): String =
    (0 until length).flatMap(i => letters.get(position + direction * i)).mkString("")

object WordSearch:
  def fromInput(input: InputData): WordSearch =
    WordSearch(Array2d.fromInput(input))


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
    if position.row >= 0 && position.row < numRows && position.column >= 0 && position.column < numColumns then
      Some(rows(position.row).charAt(position.column))
    else
      None

  def indices: Iterator[Position] =
    (0 until numRows).iterator.flatMap(column => (0 until numColumns).map(row => Position(row, column)))

object Array2d:
  def fromInput(input: InputData): Array2d =
    val rows = input.lines.toIndexedSeq
    Array2d(rows, rows.size, rows(0).length)
