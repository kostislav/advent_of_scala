package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, histogram, pattern, toSeqs}

object Day01:
  def part1(input: InputData): Int =
    val (leftValues, rightValues) = input.linesAs[ValuePair]
      .map(_.asTuple)
      .toSeqs

    leftValues.sorted
      .zip(rightValues.sorted)
      .map((value1, value2) => (value2 - value1).abs)
      .sum

  def part2(input: InputData): Int =
    val (leftValues, rightValues) = input.linesAs[ValuePair]
      .map(_.asTuple)
      .toSeqs

    val histogram = rightValues.histogram
    leftValues.map(value => value * histogram.getOrElse(value, 0))
      .sum


@pattern("{}   {}")
case class ValuePair(value1: Int, value2: Int):
  def asTuple: (Int, Int) =
    (value1, value2)
