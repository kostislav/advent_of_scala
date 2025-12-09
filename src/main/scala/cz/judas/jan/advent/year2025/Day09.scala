package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InclusiveRange, InputData, Position, pattern, slidingTuples, unorderedCombinations}

object Day09:
  def part1(input: InputData): Long =
    val tiles = input.linesAs[Position @pattern("{},{}")].toSeq
    allRectangles(tiles)
      .map(_.area)
      .max

  def part2(input: InputData): Long =
    val tiles = input.linesAs[Position @pattern("{},{}")].toSeq
    val connections = (tiles ++ Seq(tiles.head)).slidingTuples
      .map(Line.apply)
      .toSeq
    allRectangles(tiles)
      .filterNot(rectangle => connections.exists(rectangle.isNotOutside))
      .map(_.area)
      .max

  private def allRectangles(tiles: Iterable[Position]): Iterator[Rectangle] =
    tiles.unorderedCombinations
      .map(Rectangle.apply)


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