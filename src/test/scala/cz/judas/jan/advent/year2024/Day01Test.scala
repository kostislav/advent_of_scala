package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day01Test:
  private val input = InputData.fromString(
    """
      3   4
      4   3
      2   5
      1   3
      3   9
      3   3
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day01.part1(input) == 11)

  @Test
  def part2Works(): Unit =
    assert(Day01.part2(input) == 31)
