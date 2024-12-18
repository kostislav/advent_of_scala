package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day18Test:
  private val input = InputData.fromString(
    """
      5,4
      4,2
      4,5
      3,0
      2,1
      6,3
      2,4
      1,5
      0,6
      3,3
      2,6
      5,1
      1,2
      5,5
      2,5
      6,5
      1,4
      0,4
      6,4
      1,1
      6,1
      1,0
      0,5
      1,6
      2,0
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day18.part1X(input, 6, 12) == 22)

  @Test
  def part2Works(): Unit =
    assert(Day18.part2X(input, 6) == "6,1")
