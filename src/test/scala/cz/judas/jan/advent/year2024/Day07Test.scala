package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day07Test:
  private val input = InputData.fromString(
    """
      190: 10 19
      3267: 81 40 27
      83: 17 5
      156: 15 6
      7290: 6 8 6 15
      161011: 16 10 13
      192: 17 8 14
      21037: 9 7 18 13
      292: 11 6 16 20
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day07.part1(input) == 3749)

  @Test
  def part2Works(): Unit =
    assert(Day07.part2(input) == 11387)
