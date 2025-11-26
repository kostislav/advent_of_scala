package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, distance, histogram, mapAll, pattern, unzip, zipElements}

object Day01:
  def part1(input: InputData): Int =
    input.linesAs[(Int, Int) @pattern("{}   {}")]
      .unzip
      .mapAll(_.sorted)
      .zipElements
      .map(distance)
      .sum

  def part2(input: InputData): Int =
    val (left, right) = input.linesAs[(Int, Int) @pattern("{}   {}")]
      .unzip

    val histogram = right.histogram
    left.map(value => value * histogram.get(value))
      .sum
