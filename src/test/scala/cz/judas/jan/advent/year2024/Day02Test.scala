package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day02Test:
  private val input = InputData.fromString(
    """
      7 6 4 2 1
      1 2 7 8 9
      9 7 6 2 1
      1 3 2 4 5
      8 6 4 4 1
      1 3 6 7 9
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day02.part1(input) == 2)

  @Test
  def part2Works(): Unit =
    assert(Day02.part2(input) == 4)
