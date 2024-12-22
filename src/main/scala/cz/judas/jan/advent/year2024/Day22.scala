package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, applyNTimes, toMapSafe}

object Day22:
  def part1(input: InputData): Long =
    input.linesAs[Long]
      .map: initial =>
        applyNTimes(2000, initial)(generateNext)
      .sum

  def part2(input: InputData): Long =
    input.linesAs[Long]
      .flatMap: initial =>
        val prices = Iterator.unfold(initial)(current => Some((current % 10, generateNext(current))))
          .take(2001)
          .toSeq

        prices
          .sliding(2)
          .map(priceWindow => priceWindow(1) - priceWindow.head)
          .sliding(4)
          .zip(prices.drop(4))
          .toMapSafe((first, second) => first)
      .toMapSafe(_ + _)
      .values
      .max

  private def generateNext(current: Long): Long =
    val temp1 = (current ^ (current << 6)) & 16777215
    val temp2 = (temp1 ^ (temp1 >> 5)) & 16777215
    (temp2 ^ (temp2 << 11)) & 16777215
