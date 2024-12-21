package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day21Test:
  private val input = InputData.fromString(
    """
      029A
      980A
      179A
      456A
      379A
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day21.part1(input) == 126384)
