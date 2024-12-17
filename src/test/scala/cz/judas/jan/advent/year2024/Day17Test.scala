package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day17Test:
  @Test
  def part1Works(): Unit =
    val input = InputData.fromString(
      """
      Register A: 729
      Register B: 0
      Register C: 0

      Program: 0,1,5,4,3,0
    """
    )

    assert(Day17.part1(input) == "4,6,3,5,6,3,5,2,1,0")
