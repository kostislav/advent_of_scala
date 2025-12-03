package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData

object Day03:
  def part1(input: InputData): Int =
    input.lines
      .map: bank =>
        val firstDigit = bank.substring(0, bank.length - 1).max
        val secondDigit = bank.substring(bank.indexOf(firstDigit) + 1).max
        s"${firstDigit}${secondDigit}".toInt
      .sum

  def part2(input: InputData): Long =
    input.lines
      .map(bleh(_, 12).toLong)
      .sum

  private def bleh(bank: String, remaining: Int): String =
    val digit = bank.substring(0, bank.length - remaining + 1).max
    if remaining == 1 then
      digit.toString
    else
      digit.toString + bleh(bank.substring(bank.indexOf(digit) + 1), remaining - 1)

