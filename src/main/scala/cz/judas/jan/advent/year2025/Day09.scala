package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, Position, pattern, selfProduct}

object Day09:
  def part1(input: InputData): Long =
    val tiles = input.linesAs[Position @pattern("{},{}")].toSeq
    tiles.selfProduct(onlyDifferent = true)
      .map((first, second) => ((second.row.toLong - first.row).abs + 1) * ((second.column.toLong - first.column).abs + 1))
      .max

  def part2(input: InputData): Long =
    val tiles = input.linesAs[Position @pattern("{},{}")].toSeq
    val connections = (tiles ++ Seq(tiles.head)).sliding(2).map { case Seq(start, end) => (start, end) }.toSeq
    tiles.selfProduct(onlyDifferent = true)
      .map: (first, second) =>
        val top = math.min(first.row, second.row)
        val bottom = math.max(first.row, second.row)
        val left = math.min(first.column, second.column)
        val right = math.max(first.column, second.column)
        val intersects = connections.exists: (start, end) =>
          ((start.row > top && start.row < bottom) || (end.row > top && end.row < bottom) || ((end.row + start.row) / 2 > top && (end.row + start.row) / 2 < bottom)) && ((start.column > left && start.column < right) || (end.column > left && end.column < right) || ((end.column + start.column) / 2 > left && (end.column + start.column) / 2 < right))

        if intersects then
          0
        else
          (bottom.toLong - top + 1) * (right.toLong - left + 1)
      .max


