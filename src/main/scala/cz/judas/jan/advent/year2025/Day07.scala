package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{Array2d, InputData, MultiSet, Position, RelativePosition, applyNTimes}

import scala.collection.mutable

object Day07:
  def part1(input: InputData): Int =
    val diagram = input.asArray2d
    val tachyon = diagram.positionOfOnly('S')
    var columns = Set(tachyon.column)
    var numSplits = 0
    ((tachyon.row + 1) until diagram.numRows).foreach: row =>
      val newColumns = mutable.Set[Int]()
      columns.foreach: column =>
        if diagram(Position(row, column)) == '^' then
          numSplits += 1
          newColumns ++= Set(column - 1, column + 1)
        else
          newColumns += column
      columns = newColumns.toSet

    numSplits

  def part2(input: InputData): Long =
    val diagram = input.asArray2d
    val initialTachyon = diagram.positionOfOnly('S')
    applyNTimes(diagram.numRows - 1 - initialTachyon.row, MultiSet(initialTachyon)): tachyons =>
      tachyons.flatMap(tachyon => move(diagram, tachyon))
    .size

  private def move(diagram: Array2d, tachyon: Position): Seq[Position] =
    val newPosition = tachyon + RelativePosition.DOWN
    if diagram(newPosition) == '^' then
      Seq(newPosition + RelativePosition.LEFT, newPosition + RelativePosition.RIGHT)
    else
      Seq(newPosition)
