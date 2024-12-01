package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, absoluteDifference, histogram, mapAll, unzip, zipElements, given}

object Day01:
  def part1(input: InputData): Int =
    input.linesAs[Int, Int](separatedBy = "   ")
      .unzip
      .mapAll(_.sorted)
      .zipElements
      .map(absoluteDifference)
      .sum

  def part2(input: InputData): Int =
    val (left, right) = input.linesAs[Int, Int](separatedBy = "   ")
      .unzip

    val histogram = right.histogram
    left.map(value => value * histogram.get(value))
      .sum
