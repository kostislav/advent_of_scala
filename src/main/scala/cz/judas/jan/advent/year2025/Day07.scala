package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, MultiSet, Position}

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
    val tachyon = diagram.positionOfOnly('S')
    ((tachyon.row + 1) until diagram.numRows)
      .foldLeft(MultiSet(tachyon.column)): (columns, row) =>
        columns.flatMap: column =>
          if diagram(Position(row, column)) == '^' then
            Seq(column - 1, column + 1)
          else
            Seq(column)
      .size

