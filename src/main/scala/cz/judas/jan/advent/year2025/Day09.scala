package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InclusiveRange, InputData, Position, pattern, unorderedCombinations}

object Day09:
  def part1(input: InputData): Long =
    val tiles = input.linesAs[Position @pattern("{},{}")].toSeq
    tiles.unorderedCombinations
      .map(Rectangle.apply)
      .map(_.area)
      .max

  def part2(input: InputData): Long =
    val tiles = input.linesAs[Position @pattern("{},{}")].toSeq
    val connections = (tiles ++ Seq(tiles.head)).sliding(2)
      .map:
        case Seq(start, end) => Line(start, end)
      .toSeq
    tiles.unorderedCombinations
      .map(Rectangle.apply)
      .filterNot(rectangle => connections.exists(rectangle.isNotOutside))
      .map(_.area)
      .max


case class Line(start: Position, end: Position):
  def middle: Position =
    Position((start.row + end.row) / 2, (start.column + end.column) / 2)


class Rectangle(corner1: Position, corner2: Position):
  private val rows = InclusiveRange.fromUnsorted(corner1.row, corner2.row)
  private val columns = InclusiveRange.fromUnsorted(corner1.column, corner2.column)

  def isNotOutside(line: Line): Boolean =
    val points = Seq(line.start, line.end, line.middle)
    points.map(_.row).exists(rows.containsInside) && points.map(_.column).exists(columns.containsInside)

  def area: Long =
    rows.size * columns.size