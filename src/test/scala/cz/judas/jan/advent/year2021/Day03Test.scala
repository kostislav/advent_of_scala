package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day03Test:
  private val input = InputData.fromString(
    """
      00100
      11110
      10110
      10111
      10101
      01111
      00111
      11100
      10000
      11001
      00010
      01010
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day03.part1(input) == 198)

  @Test
  def part2Works(): Unit =
    assert(Day03.part2(input) == 230)

