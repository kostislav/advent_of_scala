package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, absoluteDifference, histogram, mapAll, pattern, unzip, zipElements}

object Day01:
  def part1(input: InputData): Int =
    input.linesAs[ValuePair]
      .map(_.asTuple)
      .unzip
      .mapAll(_.sorted)
      .zipElements
      .map(absoluteDifference)
      .sum

  def part2(input: InputData): Int =
    val (left, right) = input.linesAs[ValuePair]
      .map(_.asTuple)
      .unzip

    val histogram = right.histogram
    left.map(value => value * histogram.get(value))
      .sum


@pattern("{}   {}")
case class ValuePair(value1: Int, value2: Int):
  def asTuple: (Int, Int) =
    (value1, value2)
