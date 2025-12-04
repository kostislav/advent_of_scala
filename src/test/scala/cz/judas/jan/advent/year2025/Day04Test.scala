package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day04Test:
  private val input = InputData.fromString(
    """
      ..@@.@@@@.
      @@@.@.@.@@
      @@@@@.@.@@
      @.@@@@..@.
      @@.@@@@.@@
      .@@@@@@@.@
      .@.@.@.@@@
      @.@@@.@@@@
      .@@@@@@@@.
      @.@.@@@.@.
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day04.part1(input) == 13)

  @Test
  def part2Works(): Unit =
    assert(Day04.part2(input) == 43)
