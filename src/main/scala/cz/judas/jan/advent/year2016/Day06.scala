package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{AutoMap, InputData, histogram}

object Day06:
  def part1(input: InputData): String =
    val lines = input.lines.toList
    val numColumns = lines.head.length
    (0 until numColumns)
      .map: i =>
        lines
          .map(line => line(i))
          .histogram
          .mostCommon
      .mkString("")

  def part2(input: InputData): Int =
    0

