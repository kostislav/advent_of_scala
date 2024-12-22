package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day22Test:
  @Test
  def part1Works(): Unit =
    val input = InputData.fromString(
      """
      1
      10
      100
      2024
    """
    )

    assert(Day22.part1(input) == 37327623)

  @Test
  def part2Works(): Unit =
    val input = InputData.fromString(
      """
      1
      2
      3
      2024
    """
    )

    assert(Day22.part2(input) == 23)
