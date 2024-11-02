package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day01Test:
  private val input = InputData.fromString(
    """
      199
      200
      208
      210
      200
      207
      240
      269
      260
      263
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day01.part1(input) == 7)

  @Test
  def part2Works(): Unit =
    assert(Day01.part2(input) == 5)
