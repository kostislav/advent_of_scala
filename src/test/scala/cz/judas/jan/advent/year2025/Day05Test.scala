package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day05Test:
  private val input = InputData.fromString(
    """
      3-5
      10-14
      16-20
      12-18

      1
      5
      8
      11
      17
      32
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day05.part1(input) == 3)

  @Test
  def part2Works(): Unit =
    assert(Day05.part2(input) == 14L)
