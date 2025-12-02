package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern, separatedBy}

object Day02:
  def part1(input: InputData): Long =
    input.wholeAs[Seq[Range] @separatedBy(",")]
      .flatMap: range =>
        (range.low to range.high)
          .filter: number =>
            val numberString = number.toString
            val length = numberString.length
            length % 2 == 0 && numberString.substring(0, length / 2) == numberString.substring(length / 2)
      .sum

  def part2(input: InputData): Long =
    input.wholeAs[Seq[Range] @separatedBy(",")]
      .flatMap: range =>
        (range.low to range.high)
          .filter: number =>
            val numberString = number.toString
            val length = numberString.length
            (1 to length / 2)
              .exists(n => length % n == 0 && numberString == numberString.substring(0, n).repeat(length / n))
      .sum


@pattern("{}-{}")
case class Range(low: Long, high: Long)