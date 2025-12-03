package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day03Test:
  private val input = InputData.fromString(
    """
      987654321111111
      811111111111119
      234234234234278
      818181911112111
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day03.part1(input) == 357)

  @Test
  def part2Works(): Unit =
    assert(Day03.part2(input) == 3121910778619L)
