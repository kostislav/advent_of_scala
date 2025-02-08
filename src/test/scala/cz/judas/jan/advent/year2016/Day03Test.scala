package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day03Test:
  private val input = InputData.fromString(
    """
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day03.part1(input) == 0)

  @Test
  def part2Works(): Unit =
    assert(Day03.part2(input) == 0)
