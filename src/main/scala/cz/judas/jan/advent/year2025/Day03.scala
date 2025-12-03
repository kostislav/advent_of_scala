package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData

object Day03:
  def part1(input: InputData): Long =
    maxJoltage(input, 2)

  def part2(input: InputData): Long =
    maxJoltage(input, 12)

  private def maxJoltage(input: InputData, numBatteries: Int): Long =
    input.lines
      .map(bleh(_, 0, numBatteries).toLong)
      .sum

  private def bleh(bank: String, start: Int, remaining: Int): String =
    val maxIndex = (start to (bank.length - remaining)).maxBy(bank)
    val digit = bank(maxIndex)

    if remaining == 1 then
      digit.toString
    else
      digit.toString + bleh(bank, maxIndex + 1, remaining - 1)

