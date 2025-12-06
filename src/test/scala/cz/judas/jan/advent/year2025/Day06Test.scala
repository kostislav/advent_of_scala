package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day06Test:
  private val input = InputData.fromString(
    """
      123 328  51 64
       45 64  387 23
        6 98  215 314
      *   +   *   +
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day06.part1(input) == 4277556L)

  @Test
  def part2Works(): Unit =
    assert(Day06.part2(input) == 3263827L)
