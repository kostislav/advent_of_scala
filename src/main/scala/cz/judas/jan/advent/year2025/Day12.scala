package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern, separatedBy}

// 723 - no
object Day12:
  def part1(input: InputData): Int =
    val (Seq(regionString), presentStrings) = input.whole.split("\n\n").toSeq.partition(_.contains('x'))
    val presents = presentStrings.map(_.split("\n").drop(1).toSeq).map(Present.apply)
    InputData.fromString(regionString)
      .linesAs[Region]
      .count: region =>
        if region.area < region.presentCounts.zipWithIndex.map((count, index) => presents(index).area * count).sum then
          false
        else if (region.numRows / 3) * (region.numColumns / 3) >= region.presentCounts.sum then
          true
        else
          println("This is gonna be hard") // TODO
          true

  def part2(input: InputData): Int =
    0

case class Present(shape: Seq[String]):
  def area: Int =
    shape.map(_.count(_ == '#')).sum

@pattern("{}x{}: {}")
case class Region(numColumns: Int, numRows: Int, presentCounts: Seq[Int] @separatedBy(" ")):
  def area: Int =
    numColumns * numRows