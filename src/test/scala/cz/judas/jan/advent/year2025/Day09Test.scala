package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day09Test:
  private val input = InputData.fromString(
    """
      7,1
      11,1
      11,7
      9,7
      9,5
      2,5
      2,3
      7,3
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day09.part1(input) == 50L)

  @Test
  def part2Works(): Unit =
    assert(Day09.part2(input) == 24L)
