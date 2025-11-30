package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{Histogram, InputData, histogram}

object Day06:
  def part1(input: InputData): String =
    decode(input, _.mostCommon)

  def part2(input: InputData): String =
    decode(input, _.leastCommon)

  private def decode(input: InputData, selector: Histogram[Char] => Char): String =
    val lines = input.lines.toList
    val numColumns = lines.head.length
    (0 until numColumns)
      .map: i =>
        val histogram = lines
          .map(line => line(i))
          .histogram
        selector(histogram)
      .mkString("")
